{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module ElmFormat.Upgrade_0_19 (UpgradeDefinition, transform, parseUpgradeDefinition, transformModule, mergeUpgradeImports, MatchedNamespace(..)) where

import Elm.Utils ((|>))

import AST.V0_16
import AST.Listing
import AST.MatchReferences
import AST.Module (Module(Module), ImportMethod(..), DetailedListing(..))
import AST.Structure
import Control.Applicative (liftA2)
import Control.Monad (zipWithM)
import Data.Char (isUpper, isLower)
import Data.Coapplicative
import Data.Foldable
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Maybe (fromMaybe, mapMaybe, listToMaybe)
import Data.Set (Set)
import ElmFormat.ImportInfo (ImportInfo)
import ElmVersion
import Reporting.Annotation (Located(A))

import qualified Data.Indexed as I
import qualified Data.List as List
import qualified Data.Map.Strict as Dict
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified ElmFormat.ImportInfo as ImportInfo
import qualified ElmFormat.Parse
import qualified Reporting.Result as Result


elm0_19upgrade :: Text.Text
elm0_19upgrade = Text.pack $ unlines
    [ "upgrade_Basics_flip f b a ="
    , "    f a b"
    , ""
    , "upgrade_Basics_curry f a b ="
    , "    f (a, b)"
    , ""
    , "upgrade_Basics_uncurry f (a, b) ="
    , "    f a b"
    , ""
    , "upgrade_Basics_rem dividend divisor ="
    , "    remainderBy divisor dividend"
    ]


data UpgradeDefinition =
    UpgradeDefinition
        { _replacements ::
            Dict.Map
                ([UppercaseIdentifier], String)
                (ASTNS Identity (MatchedNamespace [UppercaseIdentifier]) 'ExpressionNK)
        , _typeReplacements ::
            Dict.Map
                ([UppercaseIdentifier], UppercaseIdentifier)
                ([LowercaseIdentifier], ASTNS Identity (MatchedNamespace [UppercaseIdentifier]) 'TypeNK)
        , _imports :: Dict.Map [UppercaseIdentifier] (C1 Before ImportMethod)
        }

knownContents :: UpgradeDefinition -> [UppercaseIdentifier] -> [LocalName]
knownContents upgradeDefinition ns =
    let
        expressionNameToLocalName name =
            case listToMaybe name of
                Just c | isUpper c -> Just $ CtorName $ UppercaseIdentifier name
                Just c | isLower c -> Just $ VarName $ LowercaseIdentifier name
                _ -> Nothing
    in
    (mapMaybe (expressionNameToLocalName . snd)
        $ filter ((==) ns . fst)
        $ Dict.keys
        $ _replacements upgradeDefinition
    )
    <> (fmap (TypeName . snd)
            $ filter ((==) ns . fst)
            $ Dict.keys
            $ _typeReplacements upgradeDefinition
       )


parseUpgradeDefinition :: Text.Text -> Either () UpgradeDefinition
parseUpgradeDefinition definitionText =
    case ElmFormat.Parse.parse Elm_0_19 definitionText of
        Result.Result _ (Result.Ok modu@(Module _ _ _ (C _ imports) body)) ->
            let
                importInfo = ImportInfo.fromModule (const mempty) modu

                (TopLevel topLevels) = extract $ I.unFix body

                makeName :: String -> Maybe ([UppercaseIdentifier], String)
                makeName name =
                    (\rev -> (UppercaseIdentifier <$> reverse (tail rev), head rev))
                        <$> reverse <$> splitOn '_' <$> List.stripPrefix "upgrade_" name

                makeTypeName :: UppercaseIdentifier -> Maybe ([UppercaseIdentifier], UppercaseIdentifier)
                makeTypeName (UppercaseIdentifier name) =
                    (\rev -> (UppercaseIdentifier <$> reverse (tail rev), UppercaseIdentifier $ head rev))
                        <$> reverse <$> splitOn '_' <$> List.stripPrefix "Upgrade_" name

                getExpressionReplacement ::
                    TopLevelStructure (ASTNS Located [UppercaseIdentifier] 'DeclarationNK)
                    -> Maybe
                        ( ( [UppercaseIdentifier], String )
                        , ASTNS Identity [UppercaseIdentifier] 'ExpressionNK
                        )
                getExpressionReplacement def =
                    case fmap (extract . I.unFix) def of
                        Entry (Definition (I.Fix (A _ (VarPattern (LowercaseIdentifier name)))) [] _ upgradeBody) ->
                            case makeName name of
                                Just functionName -> Just (functionName, I.convert (pure . extract) upgradeBody)
                                Nothing -> Nothing

                        Entry (Definition (I.Fix (A _ (VarPattern (LowercaseIdentifier name)))) args comments upgradeBody) ->
                            case makeName name of
                                Just functionName ->
                                    Just
                                        ( functionName
                                        , I.Fix $ pure $ Lambda (fmap (fmap $ I.convert (pure . extract)) args) comments (I.convert (pure . extract) upgradeBody) False
                                        )

                                Nothing -> Nothing

                        _ ->
                            Nothing

                getTypeReplacement ::
                    TopLevelStructure (ASTNS Located [UppercaseIdentifier] 'DeclarationNK)
                    -> Maybe
                        ( ( [UppercaseIdentifier], UppercaseIdentifier )
                        , ( [LowercaseIdentifier]
                          , ASTNS Identity (MatchedNamespace [UppercaseIdentifier]) 'TypeNK
                          )
                        )
                getTypeReplacement = \case
                    Entry (I.Fix (A _ (TypeAlias comments (C _ (NameWithArgs name args)) (C _ typ)))) ->
                        case makeTypeName name of
                            Just typeName ->
                                Just
                                    ( typeName
                                    , ( fmap extract args
                                      , matchReferences importInfo $ I.convert (pure . extract) $ typ
                                      )
                                    )

                            Nothing -> Nothing

                    _ -> Nothing
            in
            Right $ UpgradeDefinition
                -- TODO: it should be an error if any of the namespaces end up unmatched; we should require that upgrade script authors always reference imports unambiguously
                { _replacements = fmap (matchReferences importInfo) $ Dict.fromList $ Maybe.mapMaybe getExpressionReplacement topLevels
                , _typeReplacements = Dict.fromList $ Maybe.mapMaybe getTypeReplacement topLevels
                , _imports = imports
                }

        Result.Result _ (Result.Err _) ->
            Left ()


splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c s =
    case dropWhile ((==) c) s of
        [] -> []
        s' ->
            w : splitOn c s''
            where
                (w, s'') =
                    break ((==) c) s'


newtype UsageCount ns = UsageCount { _usageCount :: Dict.Map ns (Dict.Map String Int) }

instance Ord ns => Semigroup (UsageCount ns) where
    (UsageCount a) <> (UsageCount b) = UsageCount $ Dict.unionWith (Dict.unionWith (+)) a b

instance Ord ns => Monoid (UsageCount ns) where
    mempty = UsageCount $ Dict.empty
    mconcat = UsageCount . Dict.unionsWith (Dict.unionWith (+)) . fmap _usageCount


countUsages ::
    (Ord ns, Coapplicative annf) =>
    ASTNS annf ns kind
    -> UsageCount ns
countUsages =
    foldReferences
        -- XXX: TODO: type names and ctor names should be independent from one another
        (\(ns, UppercaseIdentifier typName) -> UsageCount $ Dict.singleton ns (Dict.singleton typName 1))
        (\(ns, UppercaseIdentifier ctorName) -> UsageCount $ Dict.singleton ns (Dict.singleton ctorName 1))
        countRef
    where
        countRef (VarRef ns (LowercaseIdentifier varName)) = UsageCount $ Dict.singleton ns (Dict.singleton varName 1)
        countRef (TagRef ns (UppercaseIdentifier varName)) = UsageCount $ Dict.singleton ns (Dict.singleton varName 1)
        countRef (OpRef (SymbolIdentifier varName)) = mempty -- TODO: count symbol refs


transform ::
    (Coapplicative annf, Applicative annf) => -- TODO: can this be replaced with Functor annf?
    ImportInfo [UppercaseIdentifier]
    -> ASTNS annf [UppercaseIdentifier] kind
    -> ASTNS annf [UppercaseIdentifier] kind
transform importInfo =
    case parseUpgradeDefinition elm0_19upgrade of
        Right replacements ->
            applyReferences importInfo
                . I.convert (pure . extract)
                . transform' replacements importInfo
                . matchReferences importInfo

        Left () ->
            error "Couldn't parse upgrade definition"


transformModule ::
    forall annf.
    (Applicative annf, Coapplicative annf) =>
    UpgradeDefinition
    -> Module [UppercaseIdentifier] (ASTNS annf [UppercaseIdentifier] 'TopLevelNK)
    -> Module [UppercaseIdentifier] (ASTNS annf [UppercaseIdentifier] 'TopLevelNK)
transformModule upgradeDefinition modu =
    let
        (Module a b c (C preImports originalImports) originalBody) =
            modu

        importInfo =
            -- Note: this is the info used for matching references in the
            -- source file being transformed, and should NOT include
            -- the imports merged in from the upgrade definition
            ImportInfo.fromModule (knownContents upgradeDefinition) modu

        transformBody ::
            ASTNS annf (MatchedNamespace [UppercaseIdentifier]) kind
            -> ASTNS annf (MatchedNamespace [UppercaseIdentifier]) kind
        transformBody =
            -- TODO: combine transform' and transformType into a single pass
            I.convert (pure . extract)
            . transform' upgradeDefinition importInfo
            . I.cata (transformType upgradeDefinition . I.Fix)

        expressionFromTopLevelStructure ::
            TopLevelStructure (ASTNS annf (MatchedNamespace [UppercaseIdentifier]) 'DeclarationNK)
            -> Maybe (ASTNS annf (MatchedNamespace [UppercaseIdentifier]) 'ExpressionNK)
        expressionFromTopLevelStructure structure =
            case fmap (extract . I.unFix) structure of
                Entry (Definition _ _ _ expr) -> Just expr
                _ -> Nothing

        namespacesWithReplacements =
              Set.fromList $ fmap fst $ Dict.keys $ _replacements upgradeDefinition

        usages ::
            Coapplicative annf =>
            ASTNS annf (MatchedNamespace [UppercaseIdentifier]) 'TopLevelNK
            -> Dict.Map (MatchedNamespace [UppercaseIdentifier]) Int
        usages =
            fmap (Dict.foldr (+) 0) . _usageCount . countUsages

        typeReplacementsByModule :: Dict.Map [UppercaseIdentifier] (Set UppercaseIdentifier)
        typeReplacementsByModule =
            Dict.fromListWith Set.union $ fmap (fmap Set.singleton) $ Dict.keys $ _typeReplacements upgradeDefinition

        removeTypes rem listing =
            case listing of
                OpenListing c -> OpenListing c
                ClosedListing -> ClosedListing
                ExplicitListing (DetailedListing vars ops typs) ml ->
                    let
                        typs' = Dict.withoutKeys typs rem
                    in
                    -- Note: The formatter will handle converting to a closed listing if nothing is left
                    ExplicitListing (DetailedListing vars ops typs') ml

        removeUpgradedExposings ns (C pre (ImportMethod alias exposing)) =
            C pre $
                ImportMethod alias (fmap (removeTypes $ fromMaybe Set.empty $ Dict.lookup ns typeReplacementsByModule) exposing)

        finalBody :: ASTNS annf (MatchedNamespace [UppercaseIdentifier]) 'TopLevelNK
        finalBody =
            transformBody $ matchReferences importInfo originalBody

        finalImports =
            mergeUpgradeImports
                (Dict.mapWithKey removeUpgradedExposings originalImports)
                (_imports upgradeDefinition)
                namespacesWithReplacements
                (usages finalBody)

        finalImportInfo =
            ImportInfo.fromImports (const mempty) $ fmap extract finalImports
    in
    finalBody
        |> applyReferences finalImportInfo
        |> Module a b c (C preImports finalImports)


mergeUpgradeImports ::
    Dict.Map [UppercaseIdentifier] (C1 before ImportMethod)
    -> Dict.Map [UppercaseIdentifier] (C1 before ImportMethod)
    -> Set.Set [UppercaseIdentifier]
    -> Dict.Map (MatchedNamespace [UppercaseIdentifier]) Int
    -> Dict.Map [UppercaseIdentifier] (C1 before ImportMethod)
mergeUpgradeImports originalImports upgradeImports upgradesAttempted usagesAfter =
    let
        -- uBefore ns = Maybe.fromMaybe 0 $ Dict.lookup (MatchedImport _ ns) usagesBefore
        uAfter ns =
            (Maybe.fromMaybe 0 $ Dict.lookup (MatchedImport False ns) usagesAfter)
            + (Maybe.fromMaybe 0 $ Dict.lookup (MatchedImport True ns) usagesAfter)
    in
    Dict.union
        (Dict.filterWithKey (\k _ -> uAfter k > 0 || not (Set.member k upgradesAttempted)) originalImports)
        (Dict.filterWithKey (\k _ -> uAfter k > 0) upgradeImports)


data Source
    = FromUpgradeDefinition
    | FromSource


{- An expression annotated with a Source -- this type is used throughout the upgrade transformation.

NOTE: this uses `Compose Identity ((,) Source)` instead of just `(,) Source` with the thought that maybe `Identity` could be extracted as a parameter

-}
type UAST kind = ASTNS (Compose Identity ((,) Source)) (MatchedNamespace [UppercaseIdentifier]) kind
type UExpr = UAST 'ExpressionNK


transform' ::
    Coapplicative annf =>
    UpgradeDefinition
    -> ImportInfo [UppercaseIdentifier]
    -> ASTNS annf (MatchedNamespace [UppercaseIdentifier]) kind
    -> UAST kind -- TODO: refactor to retain the original annf around ((,) Source)?
transform' upgradeDefinition importInfo =
    I.cata (simplify . applyUpgrades upgradeDefinition importInfo . I.Fix)
        . I.convert (Compose . Identity . (,) FromSource . extract)
        -- TODO: get rid of extract and keep the original annf


transformType ::
    -- TODO: can (Coapplicative annf, Applicative annf) be reduced to Functor annf?
    (Coapplicative annf, Applicative annf) =>
    UpgradeDefinition
    -> ASTNS annf (MatchedNamespace [UppercaseIdentifier]) kind
    -> ASTNS annf (MatchedNamespace [UppercaseIdentifier]) kind -- TODO: should return ((,) Source) instead of annf
transformType upgradeDefinition typ =
    case extract $ I.unFix typ of
        TypeConstruction (NamedConstructor (MatchedImport _ ctorNs, ctorName)) args ->
            case Dict.lookup (ctorNs, ctorName) (_typeReplacements upgradeDefinition) of
                Just (argOrder, newTyp) ->
                    let
                        inlines =
                            Dict.fromList $ zip argOrder (fmap extract args)
                    in
                    I.cata (inlineTypeVars inlines . I.Fix) (I.convert (pure . extract) newTyp)

                Nothing -> typ

        _ -> typ


applyUpgrades :: UpgradeDefinition -> ImportInfo [UppercaseIdentifier] -> UAST kind -> UAST kind
applyUpgrades upgradeDefinition importInfo expr =
    let
        exposed = ImportInfo._exposed importInfo
        replacements = _replacements upgradeDefinition

        replace :: Ref (MatchedNamespace [UppercaseIdentifier]) -> Maybe (ASTNS Identity (MatchedNamespace [UppercaseIdentifier]) 'ExpressionNK)
        replace var =
            case var of
                VarRef NoNamespace (LowercaseIdentifier name) ->
                    Dict.lookup ([UppercaseIdentifier "Basics"], name) replacements

                VarRef (MatchedImport _ ns) (LowercaseIdentifier name) ->
                    Dict.lookup (ns, name) replacements

                TagRef (MatchedImport _ ns) (UppercaseIdentifier name) ->
                    Dict.lookup (ns, name) replacements

                OpRef (SymbolIdentifier "!") ->
                    Just $ I.Fix $ pure $
                    Lambda
                      [makeArg "model", makeArg "cmds"] []
                      (I.Fix $ pure $ Binops
                          (makeVarRef "model")
                          [BinopsClause [] var [] (makeVarRef "cmds")]
                          False
                      )
                      False

                OpRef (SymbolIdentifier "%") ->
                    Just $ I.Fix $ pure $
                    Lambda
                      [makeArg "dividend", makeArg "modulus"] []
                      (I.Fix $ pure $ App
                          (makeVarRef "modBy")
                          [ C [] $ makeVarRef "modulus"
                          , C [] $ makeVarRef "dividend"
                          ]
                          (FAJoinFirst JoinAll)
                      )
                      False

                _ -> Nothing

        makeTuple :: Applicative annf => Int -> ASTNS annf (MatchedNamespace ns) 'ExpressionNK
        makeTuple n =
            let
                vars =
                  if n <= 26
                    then fmap (\c -> [c]) (take n ['a'..'z'])
                    else error (pleaseReport'' "UNEXPECTED TUPLE" "more than 26 elements")
            in
                I.Fix $ pure $ Lambda
                    (fmap makeArg vars)
                    []
                    (I.Fix $ pure $ Tuple (fmap (\v -> C ([], []) (makeVarRef v)) vars) False)
                    False
    in
    case runIdentity $ getCompose $ I.unFix expr of
        (_, VarExpr var) ->
            Maybe.fromMaybe expr $ fmap (I.convert (Compose . pure . (,) FromUpgradeDefinition . extract)) $ replace var

        (_, TupleFunction n) ->
            I.convert (Compose . pure . (,) FromUpgradeDefinition . runIdentity) $ makeTuple n

        (ann, ExplicitList terms' trailing multiline) ->
            let
                ha = (fmap UppercaseIdentifier ["Html", "Attributes"])
                styleExposed = Dict.lookup (VarName $ LowercaseIdentifier "style") exposed == Just ha
            in
            I.Fix $ Compose $ pure $ (,) ann $ ExplicitList (Sequence $ concat $ fmap (expandHtmlStyle styleExposed) $ sequenceToList terms') trailing multiline

        _ ->
            expr


simplify :: UAST kind -> UAST kind
simplify expr =
    let
        isElmRefactorRemove :: (Source, AST typeRef ctorRef (Ref (MatchedNamespace [UppercaseIdentifier])) getType 'ExpressionNK) -> Bool
        isElmRefactorRemove (FromUpgradeDefinition, VarExpr (VarRef (MatchedImport _ [UppercaseIdentifier "ElmRefactor"]) (LowercaseIdentifier "remove"))) = True
        isElmRefactorRemove (FromUpgradeDefinition, VarExpr (VarRef (Unmatched [UppercaseIdentifier "ElmRefactor"]) (LowercaseIdentifier "remove"))) = True
        isElmRefactorRemove _ = False
    in
    case runIdentity $ getCompose $ I.unFix expr of
        -- apply arguments to special functions (like literal lambdas)
        (source, App fn args multiline) ->
            simplifyFunctionApplication source fn args multiline

        -- Remove ElmRefactor.remove from lists
        (source, ExplicitList terms' trailing multiline) ->
            I.Fix $ Compose $ Identity $ (,) source $ ExplicitList
                (Sequence $ filter (not . isElmRefactorRemove . runIdentity . getCompose. I.unFix . extract) $ sequenceToList terms')
                trailing
                multiline

        -- Inline field access of a literal record
        (FromUpgradeDefinition, Access e field) ->
            case runIdentity $ getCompose $ I.unFix e of
                (_, Record _ fs _ _) ->
                    case find (\(Pair (C _ f) _ _) -> f == field) fs of
                        Nothing ->
                            expr
                        Just (Pair _ (C _ fieldValue) _) ->
                            fieldValue
                _ ->
                    expr

        -- reduce if expressions with a literal bool condition
        (FromUpgradeDefinition, If (IfClause (C (preCond, postCond) cond) (C (preIf, postIf) ifBody)) [] (C preElse elseBody)) ->
            destructureFirstMatch (C preCond cond)
                [ (C [] $ I.Fix $ Identity $ LiteralPattern $ Boolean True, ifBody) -- TODO: not tested
                , (C [] $ I.Fix $ Identity $ LiteralPattern $ Boolean False, elseBody)
                ]
                expr

        -- reduce case expressions
        (FromUpgradeDefinition, Case (C (pre, post) term, _) branches) ->
            let
                makeBranch :: UAST 'CaseBranchNK -> (C1 c (UAST 'PatternNK), UExpr)
                makeBranch branch =
                    case extract $ I.unFix branch of
                        (CaseBranch prePattern postPattern _ p1 b1) ->
                            (C prePattern p1, b1)
            in
            destructureFirstMatch (C pre term)
                (fmap makeBranch branches)
                expr

        _ ->
            expr


expandHtmlStyle :: Bool -> C2Eol preComma pre UExpr -> [C2Eol preComma pre UExpr]
expandHtmlStyle styleExposed (C (preComma, pre, eol) term) =
    let
        lambda fRef =
            I.convert (Compose . Identity . (,) FromUpgradeDefinition . runIdentity) $
            I.Fix $ pure $
            Lambda
                [(C [] $ I.Fix $ pure $ TuplePattern [makeArg' "a", makeArg' "b"]) ] []
                (I.Fix $ pure $ App
                    (I.Fix $ pure $ VarExpr $ fRef)
                    [ (C [] $ makeVarRef "a")
                    , (C [] $ makeVarRef "b")
                    ]
                    (FAJoinFirst JoinAll)
                )
                False

        isHtmlAttributesStyle var =
            case var of
                VarRef (MatchedImport _ [UppercaseIdentifier "Html", UppercaseIdentifier "Attributes"]) (LowercaseIdentifier "style") -> True
                VarRef NoNamespace (LowercaseIdentifier "style") -> styleExposed
                _ -> False
    in
    case extract $ I.unFix $ I.convert (runIdentity . getCompose) term of
        App (I.Fix (_, VarExpr var)) [C preStyle (I.Fix (_, ExplicitList styles trailing _))] _
          | isHtmlAttributesStyle var
          ->
            let
                convert (C (preComma', pre', eol') style) =
                    C
                        ( preComma ++ preComma'
                        , pre ++ preStyle ++ pre' ++ trailing ++ (Maybe.maybeToList $ fmap LineComment eol)
                        , eol'
                        )
                        (I.Fix $ Compose $ Identity $ (,) FromUpgradeDefinition $ App (lambda var) [C [] $ I.convert (Compose . Identity) style] (FAJoinFirst JoinAll))
            in
            fmap convert $ sequenceToList styles

        _ ->
            [C (preComma, pre, eol) term]

--
-- Generic helpers
--


pleaseReport'' :: String -> String -> String
pleaseReport'' what details =
    -- TODO: include version in the message
    "<elm-format: "++ what ++ ": " ++ details ++ " -- please report this at https://github.com/avh4/elm-format/issues >"


makeArg :: Applicative annf => String -> C1 before (ASTNS annf ns 'PatternNK)
makeArg varName =
    C [] $ I.Fix $ pure $ VarPattern $ LowercaseIdentifier varName


makeArg' :: Applicative annf => String -> C2 before after (ASTNS annf ns 'PatternNK)
makeArg' varName =
    C ([], []) (I.Fix $ pure $ VarPattern $ LowercaseIdentifier varName)


makeVarRef :: Applicative annf => String -> ASTNS annf (MatchedNamespace any) 'ExpressionNK
makeVarRef varName =
    I.Fix $ pure $ VarExpr $ VarRef NoNamespace $ LowercaseIdentifier varName


applyMappings :: Bool -> Dict.Map LowercaseIdentifier UExpr -> UExpr -> UExpr
applyMappings insertMultiline mappings =
    I.cata (simplify . I.Fix)
        . I.convert (Compose . Identity . fmap snd . getCompose)
        . I.cata (inlineVars ((==) NoNamespace) insertMultiline mappings . I.Fix)
        . I.convert (Compose . fmap ((,) False) . runIdentity . getCompose)


inlineVars ::
    forall ns ann kind.
    (ns -> Bool)
    -> Bool
    -> Dict.Map LowercaseIdentifier (ASTNS (Compose Identity ((,) ann)) ns 'ExpressionNK)
    -> ASTNS (Compose ((,) ann) ((,) Bool)) ns kind
    -> ASTNS (Compose ((,) ann) ((,) Bool)) ns kind
inlineVars isLocal insertMultiline mappings expr =
    let
        mapFst f (a, b) = (f a, b)
    in
    case extract $ I.unFix expr of
        VarExpr (VarRef ns n) | isLocal ns ->
            case Dict.lookup n mappings of
                Just e ->
                    I.Fix $ Compose $ fmap (mapFst $ const insertMultiline) $ getCompose $ I.unFix
                    $ I.convert (Compose . fmap ((,) False) . runIdentity . getCompose) e

                Nothing ->
                    expr

        Tuple terms' multiline ->
            let
                requestedMultiline (C _ (I.Fix (Compose (_, (m, _))))) = m
                newMultiline = multiline || any requestedMultiline terms'
            in
            I.Fix $ fmap (\_ -> Tuple terms' newMultiline) $ I.unFix expr

        -- TODO: handle expanding multiline in contexts other than tuples

        _ -> expr


inlineTypeVars ::
    -- TODO: can Coapplciative annf, Applicative annf be reduced to Functor annf?
    (Coapplicative annf, Applicative annf, Coapplicative annf') =>
    Dict.Map LowercaseIdentifier (ASTNS annf' ns 'TypeNK)
    -> ASTNS annf ns kind
    -> ASTNS annf ns kind
inlineTypeVars mappings typ =
    case extract $ I.unFix typ of
        TypeVariable ref ->
            case Dict.lookup ref mappings of
                Just replacement -> I.convert (pure . extract) replacement
                Nothing -> typ

        _ -> typ


destructureFirstMatch ::
    Coapplicative annf =>
    C1 before UExpr
    -> [ ( C1 before (ASTNS annf (MatchedNamespace [UppercaseIdentifier]) 'PatternNK)
         , UAST 'ExpressionNK
         )
       ]
    -> UAST 'ExpressionNK
    -> UAST 'ExpressionNK
destructureFirstMatch value choices fallback =
    -- If we find an option that Matches, use the first one we find.
    -- Otherwise, keep track of which ones *could* match -- if there is only one remaining, we can use that
    let
        resolve [] = fallback
        resolve [body] = body
        resolve _ = fallback -- TODO: could instead indicate which options the caller should keep

        destructureFirstMatch' [] othersPossible = resolve othersPossible
        destructureFirstMatch' ((pat, body):rest) othersPossible =
            case destructure pat value of
                Matches mappings ->
                    resolve (applyMappings False mappings body : othersPossible)

                CouldMatch ->
                    destructureFirstMatch' rest (body:othersPossible)

                DoesntMatch ->
                    destructureFirstMatch' rest othersPossible
    in
    destructureFirstMatch' choices []


withComments :: Comments -> UExpr -> Comments -> UExpr
withComments [] e [] = e
withComments pre e post = I.Fix $ Compose $ Identity $ (,) FromUpgradeDefinition $ Parens $ C (pre, post) e


data DestructureResult a
    = Matches a
    | CouldMatch
    | DoesntMatch
    deriving (Functor, Show)


instance Applicative DestructureResult where
    pure a = Matches a

    liftA2 f (Matches a) (Matches b) = Matches (f a b)
    liftA2 _ (Matches _) CouldMatch = CouldMatch
    liftA2 _ CouldMatch (Matches _) = CouldMatch
    liftA2 _ CouldMatch CouldMatch = CouldMatch
    liftA2 _ _ DoesntMatch = DoesntMatch
    liftA2 _ DoesntMatch _ = DoesntMatch


instance Monad DestructureResult where
    (Matches a) >>= f = f a
    CouldMatch >>= _ = CouldMatch
    DoesntMatch >>= _ = DoesntMatch


{-| Returns `Nothing` if the pattern doesn't match, or `Just` with a list of bound variables if the pattern does match. -}
destructure ::
    Coapplicative annf =>
    C1 before (ASTNS annf (MatchedNamespace [UppercaseIdentifier]) 'PatternNK)
    -> C1 before UExpr
    -> DestructureResult (Dict.Map LowercaseIdentifier UExpr)
destructure pat arg =
    let
        namespaceMatch nsd ns =
            case (nsd, ns) of
                (Unmatched nsd', MatchedImport _ ns') -> nsd' == ns'
                _ -> nsd == ns
    in
    case (fmap (extract . I.unFix) pat, fmap (extract . I.unFix) arg) of
        -- Parens in expression
        ( _
          , C preArg (Parens (C (pre, post) inner))
          )
          ->
            destructure pat (C (preArg ++ pre ++ post) inner)

        -- Parens in pattern
        ( C preVar (PatternParens (C (pre, post) inner))
          , _
          )
          ->
            destructure (C (preVar ++ pre ++ post) inner) arg

        -- Wildcard `_` pattern
        ( C _ Anything, _ ) -> Matches Dict.empty

        -- Unit
        ( C preVar (UnitPattern _)
          , C preArg (Unit _)
          )
          ->
            Matches Dict.empty

        -- Literals
        ( C preVar (LiteralPattern pat)
          , C preArg (Literal val)
          )
          ->
            -- TODO: handle ints/strings that are equal but have different representations
            if pat == val then
                Matches Dict.empty
            else
                DoesntMatch

        -- Custom type variants with no arguments
        ( C preVar (DataPattern (nsd, name) [])
          , C preArg (VarExpr (TagRef ns tag))
          )
          ->
            if name == tag && namespaceMatch nsd ns then
                Matches Dict.empty
            else
                DoesntMatch

        -- Custom type variants with arguments
        ( C preVar (DataPattern (nsd, name) argVars)
          , C preArg (App (I.Fix (Compose (Identity (_, VarExpr (TagRef ns tag))))) argValues _)
          )
          ->
            if name == tag && namespaceMatch nsd ns then
                Dict.unions <$> zipWithM destructure argVars argValues
            else
                DoesntMatch

        -- Custom type variants where pattern and value don't match in having args
        ( C _ (DataPattern _ (_:_)), C _ (VarExpr (TagRef _ _)) ) -> DoesntMatch
        ( C _ (DataPattern _ []), C _ (App (I.Fix (Compose (Identity (_, VarExpr (TagRef _ _))))) _ _) ) -> DoesntMatch

        -- Named variable pattern
        ( C preVar (VarPattern name)
          , C preArg arg'
          ) ->
            Matches $ Dict.singleton name (withComments (preVar ++ preArg) (extract arg) [])

        -- `<|` which could be parens in expression
        ( _
          , C preArg (Binops e (BinopsClause pre (OpRef (SymbolIdentifier "<|")) post arg1 : rest) ml)
          )
          ->
            destructure pat (C preArg $ I.Fix $ Compose $ pure $ (,) FromSource $ App e [(C (pre ++ post) $ I.Fix $ Compose $ pure $ (,) FromSource $ Binops arg1 rest ml)] (FAJoinFirst JoinAll))

        -- Tuple with two elements (TODO: generalize this for all tuples)
        ( C preVar (TuplePattern varItems)
          , C preArg (Tuple argItems _)
          ) ->
            case (fmap (fmap (extract . I.unFix)) varItems, argItems) of
                ( [C (preA, postA) (VarPattern nameA), C (preB, postB) (VarPattern nameB)]
                  , [C (preAe, postAe) eA, C (preBe, postBe) eB]
                  ) ->
                    Matches $ Dict.fromList
                        [ (nameA, withComments (preVar ++ preArg) (withComments (preA ++ preAe) eA (postAe ++ postA)) [])
                        , (nameB, withComments (preB ++ preBe) eB (postBe ++ postB))
                        ]

                _ -> CouldMatch

        -- Record destructuring
        ( C preVar (RecordPattern varFields)
          , C preArg (Record _ argFields _ _)
          ) ->
            let
                args :: Dict.Map LowercaseIdentifier UExpr
                args =
                    argFields
                        |> foldMap (\(Pair (C _ k) (C _ v) _) -> Dict.singleton k v)

                fieldMapping :: C2 before after LowercaseIdentifier -> Maybe (LowercaseIdentifier, UExpr)
                fieldMapping (C _ var) =
                    (,) var <$> Dict.lookup var args
            in
            Maybe.fromMaybe DoesntMatch $ fmap (Matches . Dict.fromList) $ sequence $ fmap fieldMapping varFields

        -- `as`
        ( C preVar (Alias (C _ p) (C _ varName))
          , _
          ) ->
            fmap Dict.unions $ sequence
                [ destructure (C preVar $ I.Fix $ Identity $ VarPattern varName) arg
                , destructure (C [] p) arg
                ]

        -- TODO: handle other patterns

        _ ->
            CouldMatch


simplifyFunctionApplication :: Source -> UExpr -> [C1 before UExpr] -> FunctionApplicationMultiline -> UExpr
simplifyFunctionApplication appSource fn args appMultiline =
    case (runIdentity $ getCompose $ I.unFix fn, args) of
        ((lambdaSource, Lambda (pat:restVar) preBody body multiline), arg:restArgs) ->
            case destructure pat arg of
                Matches mappings ->
                    let
                        newBody = applyMappings (appMultiline == FASplitFirst) mappings body

                        newMultiline =
                            case appMultiline of
                                FASplitFirst -> FASplitFirst
                                FAJoinFirst SplitAll -> FASplitFirst
                                FAJoinFirst JoinAll -> FAJoinFirst JoinAll
                    in
                    case restVar of
                        [] ->
                            -- we applied the argument and none are left, so remove the lambda
                            I.Fix $ Compose $ pure $ (,) appSource $ App
                                (withComments preBody newBody [])
                                restArgs
                                newMultiline

                        _:_ ->
                            -- we applied this argument; try to apply the next argument
                            simplifyFunctionApplication appSource (I.Fix $ Compose $ pure $ (,) lambdaSource $ Lambda restVar preBody newBody multiline) restArgs newMultiline
                _ ->
                    -- failed to destructure the next argument, so stop
                    I.Fix $ Compose $ pure $ (,) appSource $ App fn args appMultiline


        (_, []) -> fn

        _ -> I.Fix $ Compose $ pure $ (,) appSource $ App fn args appMultiline
