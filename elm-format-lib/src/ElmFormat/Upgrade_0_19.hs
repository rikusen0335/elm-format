{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module ElmFormat.Upgrade_0_19 (Transformation(..), applyTransformation, UpgradeDefinition, transform, parseUpgradeDefinition, transformModule, MatchedNamespace(..)) where

import Elm.Utils ((|>))

import AST.V0_16
import AST.Listing
import AST.MatchReferences
import AST.Module (Module(Module), UserImport, ImportMethod(..), DetailedListing(..))
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
import ElmFormat.KnownContents (KnownContents)
import ElmVersion
import Reporting.Annotation (Located(A))

import qualified Data.Indexed as I
import qualified Data.List as List
import qualified Data.Map.Strict as Dict
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified ElmFormat.ImportInfo as ImportInfo
import qualified ElmFormat.KnownContents as KnownContents
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

knownContents :: UpgradeDefinition -> KnownContents
knownContents upgradeDefinition =
    let
        expressionNameToLocalName name =
            case listToMaybe name of
                Just c | isUpper c -> Just $ CtorName $ UppercaseIdentifier name
                Just c | isLower c -> Just $ VarName $ LowercaseIdentifier name
                _ -> Nothing
    in
    KnownContents.fromFunction $
        \ns ->
            Just $ -- TODO: always returning Just here is probably incorret
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
                importInfo = ImportInfo.fromModule mempty modu -- TODO: knownContents

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


data Transformation
    = Upgrade UpgradeDefinition
    | ApplyImport UserImport


applyTransformation ::
    forall annf.
    (Applicative annf, Coapplicative annf) =>
    Transformation
    -> Module [UppercaseIdentifier] (ASTNS annf [UppercaseIdentifier] 'TopLevelNK)
    -> Module [UppercaseIdentifier] (ASTNS annf [UppercaseIdentifier] 'TopLevelNK)
applyTransformation = \case
    Upgrade upgradeDefinition ->
        transformModule upgradeDefinition

    ApplyImport (C c name, method) ->
        -- TODO: are there cases where we need to combine all new imports into a single pass? (like swapping aliases)
        applyImports $ Dict.singleton name (C c method)


applyImports ::
    (Applicative annf, Coapplicative annf) =>
    Dict.Map [UppercaseIdentifier] (C1 Before ImportMethod)
    -> Module [UppercaseIdentifier] (ASTNS annf [UppercaseIdentifier] nk)
    -> Module [UppercaseIdentifier] (ASTNS annf [UppercaseIdentifier] nk)
applyImports importsToApply modu =
    let
        (Module a b c (C preImports originalImports) originalBody) =
            modu

        knownModuleContents =
            mempty
        importInfo =
            ImportInfo.fromModule knownModuleContents modu

        finalBody =
            originalBody
                |> matchReferences importInfo

        finalImports =
            Dict.union importsToApply originalImports
                |> removeUnusedImports (const False) (usages finalBody)

        finalImportInfo =
            ImportInfo.fromImports knownModuleContents $ fmap extract finalImports
    in
    finalBody
        |> applyReferences finalImportInfo
        |> Module a b c (C preImports finalImports)


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

        typeReplacementsByModule :: Dict.Map [UppercaseIdentifier] (Set UppercaseIdentifier)
        typeReplacementsByModule =
            Dict.fromListWith Set.union $ fmap (fmap Set.singleton) $ Dict.keys $ _typeReplacements upgradeDefinition

        removeUpgradedExposings ns (C pre (ImportMethod alias exposing)) =
            C pre $
                ImportMethod alias (fmap (removeTypes $ fromMaybe Set.empty $ Dict.lookup ns typeReplacementsByModule) exposing)

        finalBody :: ASTNS annf (MatchedNamespace [UppercaseIdentifier]) 'TopLevelNK
        finalBody =
            originalBody
                |> matchReferences importInfo
                -- TODO: combine transform' and transformType into a single pass
                |> I.cata (transformType upgradeDefinition . I.Fix)
                |> transform' upgradeDefinition importInfo
                |> I.convert (pure . extract)


        importsNotTouchedByUpgrade =
            Set.difference
                (Dict.keysSet $ originalImports)
                (Set.fromList $ fmap fst $ Dict.keys $ _replacements upgradeDefinition)

        finalImports =
            Dict.union
                (Dict.mapWithKey removeUpgradedExposings originalImports)
                (_imports upgradeDefinition)
                |> removeUnusedImports (flip Set.member importsNotTouchedByUpgrade) (usages finalBody)

        finalImportInfo =
            ImportInfo.fromImports mempty $ fmap extract finalImports -- TODO: knownContents
    in
    finalBody
        |> applyReferences finalImportInfo
        |> Module a b c (C preImports finalImports)


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

usages ::
    (Coapplicative annf, Ord ns) =>
    ASTNS annf (MatchedNamespace ns) nk
    -> Dict.Map (MatchedNamespace ns) Int
usages =
    fmap (Dict.foldr (+) 0) . _usageCount . countUsages


removeUnusedImports ::
    ([UppercaseIdentifier] -> Bool)
    -> Dict.Map (MatchedNamespace [UppercaseIdentifier]) Int
    -> Dict.Map [UppercaseIdentifier] (C1 before ImportMethod)
    -> Dict.Map [UppercaseIdentifier] (C1 before ImportMethod)
removeUnusedImports keepAnyway usages originalImports =
    let
        potentialUsages :: Dict.Map [UppercaseIdentifier] Int
        potentialUsages =
            Dict.foldMapWithKey (\ns count ->
                                   case ns of
                                       UnmatchedUnqualified nss ->
                                           List.map (\n -> (n, count)) nss
                                               |> Dict.fromList
                                       _ -> mempty
                                   )
                                  usages
        uAfter ns =
            (Maybe.fromMaybe 0 $ Dict.lookup (MatchedImport False ns) usages)
            + (Maybe.fromMaybe 0 $ Dict.lookup (MatchedImport True ns) usages)
            + (Maybe.fromMaybe 0 $ Dict.lookup ns potentialUsages)
    in
    Dict.filterWithKey (\k _ -> uAfter k > 0 || keepAnyway k) originalImports


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
                VarRef (UnmatchedUnqualified _) (LowercaseIdentifier name) ->
                    -- TODO: include default imports in the matchReferences, so this isn't needed here
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
            I.Fix $ Compose $ pure $ (,) ann $ ExplicitList (Sequence $ concat $ fmap expandHtmlStyle $ sequenceToList terms') trailing multiline

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

        -- Evaluate certain binary operators when the args are literals
        (FromUpgradeDefinition, Binops (I.Fix (Compose (Identity (_, Literal left)))) [BinopsClause preOp (OpRef (SymbolIdentifier "==")) postOp (I.Fix (Compose (Identity (_, Literal right))))] _) ->
            I.Fix $ Compose $ Identity $ (,) FromUpgradeDefinition $ Literal $ Boolean (left == right)

        -- Evaluate `++` when the args are literal lists
        (source, Binops (I.Fix (Compose (Identity (_, ExplicitList left postLeft mlLeft)))) [BinopsClause preOp (OpRef (SymbolIdentifier "++")) postOp (I.Fix (Compose (Identity (_, ExplicitList right postRight mlRight))))] _) ->
            I.Fix $ Compose $ Identity $ (,) FromUpgradeDefinition $ ExplicitList (left <> right) postRight (mlLeft <> mlRight)

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


expandHtmlStyle :: C2Eol preComma pre UExpr -> [C2Eol preComma pre UExpr]
expandHtmlStyle (C (preComma, pre, eol) term) =
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
    in
    case extract $ I.unFix $ I.convert (runIdentity . getCompose) term of
        App (I.Fix (_, VarExpr var@(VarRef (MatchedImport _ [UppercaseIdentifier "Html", UppercaseIdentifier "Attributes"]) (LowercaseIdentifier "style")))) [C preStyle (I.Fix (_, ExplicitList styles trailing _))] _
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
    I.Fix $ pure $ VarExpr $ VarRef Local $ LowercaseIdentifier varName


applyMappings :: Bool -> Dict.Map LowercaseIdentifier UExpr -> UExpr -> UExpr
applyMappings insertMultiline mappings =
    I.cata (simplify . I.Fix)
        . I.convert (Compose . Identity . fmap snd . getCompose)
        . I.cata (inlineVars ((==) Local) insertMultiline mappings . I.Fix)
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

        ( (FromUpgradeDefinition, VarExpr (VarRef (MatchedImport _ [UppercaseIdentifier "List"]) (LowercaseIdentifier "filterMap")))
          , (C preIdentity (I.Fix (Compose (Identity (FromUpgradeDefinition, VarExpr (VarRef (MatchedImport _ [UppercaseIdentifier "Basics"]) (LowercaseIdentifier "identity")))))))
            : (C preArg (I.Fix (Compose (Identity (listSource, ExplicitList terms _ ml)))))
            : restArgs
          ) ->
            let
                -- returns Nothing if the term can't be prcoessed and the simplification must abort
                -- returns (Just Nothing) if the term should be removed
                -- returns (Just (Just t)) if the term should be replaced with t
                filterTerm :: Commented c (UAST 'ExpressionNK) -> Maybe (Maybe (Commented c (UAST 'ExpressionNK)))
                filterTerm = \case
                    C c (I.Fix (Compose (Identity (_, VarExpr (TagRef (MatchedImport _ [UppercaseIdentifier "Maybe"]) (UppercaseIdentifier "Nothing")))))) ->
                        -- TODO: retain comments
                        Just $ Nothing

                    C c (I.Fix (Compose (Identity (_, App (I.Fix (Compose (Identity (_, VarExpr (TagRef (MatchedImport _ [UppercaseIdentifier "Maybe"]) (UppercaseIdentifier "Just")))))) [C preVal e] _)))) ->
                        Just $ Just $ C c e -- TODO: use preVal

                    C c (I.Fix (Compose (Identity (_, Binops (I.Fix (Compose (Identity (_, VarExpr (TagRef (MatchedImport _ [UppercaseIdentifier "Maybe"]) (UppercaseIdentifier "Just")))))) [BinopsClause preOp (OpRef (SymbolIdentifier "<|")) postOp e] _)))) ->
                        Just $ Just $ C c e -- TODO: use preOp, postOp

                    C c (I.Fix (Compose (Identity (_, Binops e [BinopsClause preOp (OpRef (SymbolIdentifier "|>")) postOp (I.Fix (Compose (Identity (_, VarExpr (TagRef (MatchedImport _ [UppercaseIdentifier "Maybe"]) (UppercaseIdentifier "Just"))))))] _)))) ->
                        Just $ Just $ C c e -- TODO: use preOp, postOp

                    _ -> Nothing
            in
            case mapM filterTerm $ sequenceToList terms of
                Nothing ->
                    -- it can't be simplified
                    I.Fix $ Compose $ pure $ (,) appSource $ App fn args appMultiline

                Just newTerms ->
                    -- TODO: use restArgs
                    I.Fix $ Compose $ pure $ (,) listSource $ ExplicitList (Sequence $ mapMaybe id newTerms) [] ml

        ( (FromUpgradeDefinition, VarExpr (VarRef (MatchedImport _ [UppercaseIdentifier "Maybe"]) (LowercaseIdentifier "map")))
          , (C preF _)
            : (C preArg m@(I.Fix (Compose (Identity (_, VarExpr (TagRef (MatchedImport _ [UppercaseIdentifier "Maybe"]) (UppercaseIdentifier "Nothing")))))))
            : restArgs
          ) ->
            -- TODO: use restArgs
            -- TODO: use preArg, preF
            m

        ( (mapSource@FromUpgradeDefinition, VarExpr (VarRef (MatchedImport _ [UppercaseIdentifier "Maybe"]) (LowercaseIdentifier "map")))
          , (C preF f)
            : (C preJust (I.Fix (Compose (Identity (appSource, App just@(I.Fix (Compose (Identity (_, VarExpr (TagRef (MatchedImport _ [UppercaseIdentifier "Maybe"]) (UppercaseIdentifier "Just")))))) [C preArg e] ml)))))
            : restArgs
          ) ->
            -- TODO: use restArgs
            -- TODO: use preJust, preF
            I.Fix $ Compose $ pure $ (,) appSource $ App
                just
                [ C [] $ I.Fix $ Compose $ pure $ (,) mapSource $ App f [C preArg e] (FAJoinFirst JoinAll) ]
                ml

        _ -> I.Fix $ Compose $ pure $ (,) appSource $ App fn args appMultiline
