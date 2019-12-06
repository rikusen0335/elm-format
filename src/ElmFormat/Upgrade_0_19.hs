{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module ElmFormat.Upgrade_0_19 (UpgradeDefinition, transform, parseUpgradeDefinition, transformModule, mergeUpgradeImports, MatchedNamespace(..)) where

import Elm.Utils ((|>))

import AST.V0_16
import AST.Declaration (Declaration(..), TopLevelStructure(..))
import AST.Expression
import AST.MatchReferences
import AST.Module (Module(Module), ImportMethod(..), DetailedListing(..))
import AST.Pattern
import AST.Structure
import AST.Variable
import Control.Applicative (liftA2)
import Control.Monad (zipWithM)
import Data.Coapplicative
import Data.Functor.Identity
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import ElmFormat.ImportInfo (ImportInfo)
import ElmFormat.Mapping
import ElmVersion
import Reporting.Annotation (Located(A))

import qualified Data.List as List
import qualified Data.Map.Strict as Dict
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified ElmFormat.ImportInfo as ImportInfo
import qualified ElmFormat.Parse
import qualified ElmFormat.Version
import qualified Reporting.Annotation as RA
import qualified Reporting.Region as Region
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
                (FixASTNS Expression Identity (MatchedNamespace [UppercaseIdentifier]))
        , _typeReplacements ::
            Dict.Map
                ([UppercaseIdentifier], UppercaseIdentifier)
                ([LowercaseIdentifier], FixASTNS Typ Identity (MatchedNamespace [UppercaseIdentifier]))
        , _imports :: Dict.Map [UppercaseIdentifier] (C1 Before ImportMethod)
        }

knownContents :: UpgradeDefinition -> [UppercaseIdentifier] -> DetailedListing
knownContents upgradeDefinition ns =
    DetailedListing
        { values =
            Dict.fromList
            $ fmap (flip (,) (C ([], []) ()) . LowercaseIdentifier . snd)
            $ filter ((==) ns . fst)
            $ Dict.keys
            $ _replacements upgradeDefinition
        , operators = mempty
        , types =
            Dict.fromList
            $ fmap (flip (,) (C ([], []) (C [] ClosedListing)) . snd)
            $ filter ((==) ns . fst)
            $ Dict.keys
            $ _typeReplacements upgradeDefinition
        }


parseUpgradeDefinition :: Text.Text -> Either () UpgradeDefinition
parseUpgradeDefinition definitionText =
    case ElmFormat.Parse.parse Elm_0_19 definitionText of
        Result.Result _ (Result.Ok modu@(Module _ _ _ (C _ imports) body)) ->
            let
                importInfo = ImportInfo.fromModule (const mempty) modu

                makeName :: String -> Maybe ([UppercaseIdentifier], String)
                makeName name =
                    (\rev -> (UppercaseIdentifier <$> reverse (tail rev), head rev))
                        <$> reverse <$> splitOn '_' <$> List.stripPrefix "upgrade_" name

                makeTypeName :: UppercaseIdentifier -> Maybe ([UppercaseIdentifier], UppercaseIdentifier)
                makeTypeName (UppercaseIdentifier name) =
                    (\rev -> (UppercaseIdentifier <$> reverse (tail rev), UppercaseIdentifier $ head rev))
                        <$> reverse <$> splitOn '_' <$> List.stripPrefix "Upgrade_" name

                getExpressionReplacement ::
                    TopLevelStructure (Located (ASTNS Declaration Located [UppercaseIdentifier]))
                    -> Maybe (([UppercaseIdentifier], String), (FixASTNS Expression Identity [UppercaseIdentifier]))
                getExpressionReplacement def =
                    case def of
                        Entry (A _ (Definition (FixAST (A _ (VarPattern (LowercaseIdentifier name)))) [] _ upgradeBody)) ->
                            case makeName name of
                                Just functionName -> Just (functionName, convertFix (pure . extract) upgradeBody)
                                Nothing -> Nothing

                        Entry (A _ (Definition (FixAST (A _ (VarPattern (LowercaseIdentifier name)))) args comments upgradeBody)) ->
                            case makeName name of
                                Just functionName ->
                                    Just
                                        ( functionName
                                        , FixAST $ pure $ Lambda (fmap (fmap $ convertFix (pure . extract)) args) comments (convertFix (pure . extract) upgradeBody) False
                                        )

                                Nothing -> Nothing

                        _ ->
                            Nothing

                getTypeReplacement ::
                    TopLevelStructure (Located (ASTNS Declaration Located [UppercaseIdentifier]))
                    -> Maybe (([UppercaseIdentifier], UppercaseIdentifier) ,([LowercaseIdentifier], FixASTNS Typ Identity (MatchedNamespace [UppercaseIdentifier])))
                getTypeReplacement = \case
                    Entry (A _ (TypeAlias comments (C _ (name, args)) (C _ typ))) ->
                        case makeTypeName name of
                            Just typeName ->
                                Just (typeName, (fmap extract args, FixAST $ fmap (matchReferences importInfo) $ unFixAST $ convertFix (pure . extract) $ typ))

                            Nothing -> Nothing

                    _ -> Nothing
            in
            Right $ UpgradeDefinition
                -- TODO: it should be an error if any of the namespaces end up unmatched; we should require that upgrade script authors always reference imports unambiguously
                { _replacements = fmap (FixAST . fmap (matchReferences importInfo) . unFixAST) $ Dict.fromList $ Maybe.mapMaybe getExpressionReplacement body
                , _typeReplacements = Dict.fromList $ Maybe.mapMaybe getTypeReplacement body
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
    -- NOTE: (FixAST Expression) could be generalized to (Go' fix => fix), but then the return type
    -- would inconveniently become `Seed fix (UsageCount ns) (UsageCount ns) (UsageCount ns)`
    FixAST Expression annf (ns, UppercaseIdentifier) (ns, UppercaseIdentifier) (Ref ns)
    -> UsageCount ns
countUsages =
    cataReferences
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
    -> FixASTNS Expression annf [UppercaseIdentifier]
    -> FixASTNS Expression annf [UppercaseIdentifier]
transform importInfo =
    case parseUpgradeDefinition elm0_19upgrade of
        Right replacements ->
            (FixAST . fmap (applyReferences importInfo) . unFixAST)
                . convertFix (pure . extract)
                . transform' replacements importInfo
                . (FixAST . fmap (matchReferences importInfo) . unFixAST)

        Left () ->
            error "Couldn't parse upgrade definition"


transformModule ::
    forall annf.
    (Applicative annf, Coapplicative annf) =>
    UpgradeDefinition
    -> ASTNS (Module annf [UppercaseIdentifier]) annf [UppercaseIdentifier]
    -> ASTNS (Module annf [UppercaseIdentifier]) annf [UppercaseIdentifier]
transformModule upgradeDefinition modu =
    let
        (Module a b c (C preImports originalImports) originalBody') =
            modu

        importInfo =
            -- Note: this is the info used for matching references in the
            -- source file being transformed, and should NOT include
            -- the imports merged in from the upgrade definition
            ImportInfo.fromModule (knownContents upgradeDefinition) modu

        transformDeclaration ::
            ASTNS Declaration annf (MatchedNamespace [UppercaseIdentifier])
            -> ASTNS Declaration annf (MatchedNamespace [UppercaseIdentifier])
        transformDeclaration =
            -- TODO: should do a single cataAll instead of map with separate cata for type and expr
            mapAll id id id
                id
                (cataAll FixAST (transformType upgradeDefinition . FixAST) FixAST)
                (convertFix (pure . extract) . transform' upgradeDefinition importInfo)

        expressionFromTopLevelStructure structure =
            case fmap extract structure of
                Entry (Definition _ _ _ expr) -> Just expr
                _ -> Nothing

        namespacesWithReplacements =
              Set.fromList $ fmap fst $ Dict.keys $ _replacements upgradeDefinition

        usages ::
            Coapplicative annf =>
            [TopLevelStructure (annf (ASTNS Declaration annf (MatchedNamespace [UppercaseIdentifier])))]
            -> Dict.Map (MatchedNamespace [UppercaseIdentifier]) Int
        usages body =
            let
                collectExprs = Maybe.mapMaybe expressionFromTopLevelStructure body
                usages' = _usageCount $ mconcat $ fmap countUsages collectExprs
            in
            fmap (Dict.foldr (+) 0) usages'

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

        originalBody :: [TopLevelStructure (annf (ASTNS Declaration annf (MatchedNamespace [UppercaseIdentifier])))]
        originalBody =
            fmap (fmap $ fmap $ matchReferences importInfo) originalBody'

        finalBody :: [TopLevelStructure (annf (ASTNS Declaration annf (MatchedNamespace [UppercaseIdentifier])))]
        finalBody =
            fmap (fmap $ fmap transformDeclaration) originalBody

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
        |> fmap (fmap $ fmap $ applyReferences finalImportInfo)
        |> Module a b c (C preImports finalImports)


mergeUpgradeImports ::
    Dict.Map [UppercaseIdentifier] (C1 before ImportMethod)
    -> Dict.Map [UppercaseIdentifier] (C1 before ImportMethod)
    -> Set.Set [UppercaseIdentifier]
    -> Dict.Map (MatchedNamespace [UppercaseIdentifier]) Int
    -> Dict.Map [UppercaseIdentifier] (C1 before ImportMethod)
mergeUpgradeImports originalImports upgradeImports upgradesAttempted usagesAfter =
    let
        -- uBefore ns = Maybe.fromMaybe 0 $ Dict.lookup (MatchedImport ns) usagesBefore
        uAfter ns = Maybe.fromMaybe 0 $ Dict.lookup (MatchedImport ns) usagesAfter
    in
    Dict.union
        (Dict.filterWithKey (\k _ -> uAfter k > 0 || not (Set.member k upgradesAttempted)) originalImports)
        (Dict.filterWithKey (\k _ -> uAfter k > 0) upgradeImports)


data Source
    = FromUpgradeDefinition
    | FromSource


{- An expression annotated with a Source -- this type is used throughout the upgrade transformation. -}
type UExpr = FixASTNS Expression ((,) Source) (MatchedNamespace [UppercaseIdentifier])


transform' ::
    Coapplicative annf =>
    UpgradeDefinition
    -> ImportInfo [UppercaseIdentifier]
    -> FixASTNS Expression annf (MatchedNamespace [UppercaseIdentifier])
    -> FixASTNS Expression ((,) Source) (MatchedNamespace [UppercaseIdentifier]) -- TODO: refactor to retain the original annf
transform' upgradeDefinition importInfo =
    cataAll FixAST FixAST (simplify . applyUpgrades upgradeDefinition importInfo . FixAST)
        . convertFix ((,) FromSource . extract)


transformType ::
    -- TODO: can (Coapplicative annf, Applicative annf) be reduced to Functor annf?
    (Coapplicative annf, Applicative annf) =>
    UpgradeDefinition
    -> FixASTNS Typ annf (MatchedNamespace [UppercaseIdentifier])
    -> FixASTNS Typ annf (MatchedNamespace [UppercaseIdentifier])
transformType upgradeDefinition typ =
    case extract $ unFixAST typ of
        TypeConstruction (NamedConstructor (MatchedImport ctorNs, ctorName)) args ->
            case Dict.lookup (ctorNs, ctorName) (_typeReplacements upgradeDefinition) of
                Just (argOrder, newTyp) ->
                    let
                        inlines =
                            Dict.fromList $ zip argOrder (fmap extract args)
                    in
                    cataAll FixAST (inlineTypeVars inlines . FixAST) FixAST (convertFix (pure . extract) newTyp)

                Nothing -> typ

        _ -> typ


applyUpgrades :: UpgradeDefinition -> ImportInfo [UppercaseIdentifier] -> UExpr -> UExpr
applyUpgrades upgradeDefinition importInfo expr =
    let
        exposed = ImportInfo._exposed importInfo
        replacements = _replacements upgradeDefinition

        replace :: Ref (MatchedNamespace [UppercaseIdentifier]) -> Maybe (FixASTNS Expression Identity (MatchedNamespace [UppercaseIdentifier]))
        replace var =
            case var of
                VarRef NoNamespace (LowercaseIdentifier name) ->
                    Dict.lookup ([UppercaseIdentifier "Basics"], name) replacements

                VarRef (MatchedImport ns) (LowercaseIdentifier name) ->
                    Dict.lookup (ns, name) replacements

                TagRef (MatchedImport ns) (UppercaseIdentifier name) ->
                    Dict.lookup (ns, name) replacements

                OpRef (SymbolIdentifier "!") ->
                    Just $ FixAST $ pure $
                    Lambda
                      [makeArg "model", makeArg "cmds"] []
                      (FixAST $ pure $ Binops
                          (makeVarRef "model")
                          [BinopsClause [] var [] (makeVarRef "cmds")]
                          False
                      )
                      False

                OpRef (SymbolIdentifier "%") ->
                    Just $ FixAST $ pure $
                    Lambda
                      [makeArg "dividend", makeArg "modulus"] []
                      (FixAST $ pure $ App
                          (makeVarRef "modBy")
                          [ C [] $ makeVarRef "modulus"
                          , C [] $ makeVarRef "dividend"
                          ]
                          (FAJoinFirst JoinAll)
                      )
                      False

                _ -> Nothing

        makeTuple :: Applicative annf => Int -> FixASTNS Expression annf (MatchedNamespace ns)
        makeTuple n =
            let
                vars =
                  if n <= 26
                    then fmap (\c -> [c]) (take n ['a'..'z'])
                    else error (pleaseReport'' "UNEXPECTED TUPLE" "more than 26 elements")
            in
                FixAST $ pure $ Lambda
                    (fmap makeArg vars)
                    []
                    (FixAST $ pure $ AST.Expression.Tuple (fmap (\v -> C ([], []) (makeVarRef v)) vars) False)
                    False
    in
    case unFixAST expr of
        (_, VarExpr var) ->
            Maybe.fromMaybe expr $ fmap (convertFix ((,) FromUpgradeDefinition . extract)) $ replace var

        (_, TupleFunction n) ->
            convertFix ((,) FromUpgradeDefinition . runIdentity) $ makeTuple n

        (ann, ExplicitList terms' trailing multiline) ->
            let
                ha = (fmap UppercaseIdentifier ["Html", "Attributes"])
                styleExposed = Dict.lookup (LowercaseIdentifier "style") exposed == Just ha
            in
            FixAST $ (,) ann $ ExplicitList (concat $ fmap (expandHtmlStyle styleExposed) $ terms') trailing multiline

        _ ->
            expr


simplify :: UExpr -> UExpr
simplify expr =
    let
        isElmFixRemove (C _ (FixAST (FromUpgradeDefinition, VarExpr (VarRef (MatchedImport [UppercaseIdentifier "ElmFix"]) (LowercaseIdentifier "remove")))))= True
        isElmFixRemove (C _ (FixAST (FromUpgradeDefinition, VarExpr (VarRef (Unmatched [UppercaseIdentifier "ElmFix"]) (LowercaseIdentifier "remove")))))= True
        isElmFixRemove _ = False
    in
    case unFixAST expr of
        -- apply arguments to special functions (like literal lambdas)
        (source, App fn args multiline) ->
            simplifyFunctionApplication source fn args multiline

        -- Remove ElmFix.remove from lists
        (source, ExplicitList terms' trailing multiline) ->
            FixAST $ (,) source $ ExplicitList
                (filter (not . isElmFixRemove) terms')
                trailing
                multiline

        -- Inline field access of a literal record
        (FromUpgradeDefinition, Access e field) ->
            case unFixAST e of
                (_, AST.Expression.Record _ fs _ _) ->
                    case List.find (\(C _ (Pair (C _ f) _ _)) -> f == field) fs of
                        Nothing ->
                            expr
                        Just (C _ (Pair _ (C _ fieldValue) _)) ->
                            fieldValue
                _ ->
                    expr

        -- reduce if expressions with a literal bool condition
        (FromUpgradeDefinition, If (IfClause (C (preCond, postCond) cond) (C (preIf, postIf) ifBody)) [] (C preElse elseBody)) ->
            destructureFirstMatch (C preCond cond)
                [ (C [] $ FixAST $ Identity $ AST.Pattern.Literal $ Boolean True, ifBody) -- TODO: not tested
                , (C [] $ FixAST $ Identity $ AST.Pattern.Literal $ Boolean False, elseBody)
                ]
                expr

        -- reduce case expressions
        (FromUpgradeDefinition, Case (C (pre, post) term, _) branches) ->
            let
                makeBranch (CaseBranch prePattern postPattern _ p1 b1) =
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
            convertFix ((,) FromUpgradeDefinition . extract) $
            FixAST $ Identity $
            Lambda
                [(C [] $ FixAST $ pure $ AST.Pattern.Tuple [makeArg' "a", makeArg' "b"]) ] []
                (FixAST $ pure $ App
                    (FixAST $ pure $ VarExpr $ fRef)
                    [ (C [] $ makeVarRef "a")
                    , (C [] $ makeVarRef "b")
                    ]
                    (FAJoinFirst JoinAll)
                )
                False

        isHtmlAttributesStyle var =
            case var of
                VarRef (MatchedImport [UppercaseIdentifier "Html", UppercaseIdentifier "Attributes"]) (LowercaseIdentifier "style") -> True
                VarRef NoNamespace (LowercaseIdentifier "style") -> styleExposed
                _ -> False
    in
    case extract $ unFixAST term of
        App (FixAST (_, VarExpr var)) [C preStyle (FixAST (_, ExplicitList styles trailing _))] _
          | isHtmlAttributesStyle var
          ->
            let
                convert (C (preComma', pre', eol') style) =
                    C
                        ( preComma ++ preComma'
                        , pre ++ preStyle ++ pre' ++ trailing ++ (Maybe.maybeToList $ fmap LineComment eol)
                        , eol'
                        )
                        (FixAST $ (,) FromUpgradeDefinition $ App (lambda var) [C [] style] (FAJoinFirst JoinAll))
            in
            fmap convert styles

        _ ->
            [C (preComma, pre, eol) term]

--
-- Generic helpers
--


pleaseReport'' :: String -> String -> String
pleaseReport'' what details =
    "<elm-format-" ++ ElmFormat.Version.asString ++ ": "++ what ++ ": " ++ details ++ " -- please report this at https://github.com/avh4/elm-format/issues >"



nowhere :: Region.Position
nowhere =
    Region.Position 0 0


noRegion' :: Region.Region
noRegion' =
    Region.Region nowhere nowhere


noRegion :: a -> RA.Located a
noRegion =
    RA.at nowhere nowhere


makeArg :: Applicative annf => String -> C1 before (FixASTNS Pattern annf ns)
makeArg varName =
    C [] $ FixAST $ pure $ VarPattern $ LowercaseIdentifier varName


makeArg' :: Applicative annf => String -> C2 before after (FixASTNS Pattern annf ns)
makeArg' varName =
    C ([], []) (FixAST $ pure $ VarPattern $ LowercaseIdentifier varName)


makeVarRef :: Applicative annf => String -> FixASTNS Expression annf (MatchedNamespace any)
makeVarRef varName =
    FixAST $ pure $ VarExpr $ VarRef NoNamespace $ LowercaseIdentifier varName


applyMappings :: Bool -> Dict.Map LowercaseIdentifier UExpr -> UExpr -> UExpr
applyMappings insertMultiline mappings =
    cataAll FixAST FixAST (simplify . FixAST)
        . convertFix (\((_, ann), x) -> (ann, x))
        . cataAll FixAST FixAST (inlineVars ((==) NoNamespace) insertMultiline mappings . FixAST)
        . convertFix (\(ann, x) -> ((False, ann), x))


inlineVars ::
    (ns -> Bool)
    -> Bool
    -> Dict.Map LowercaseIdentifier (FixASTNS Expression ((,) ann) ns)
    -> FixASTNS Expression ((,) (Bool, ann)) ns
    -> FixASTNS Expression ((,) (Bool, ann)) ns
inlineVars isLocal insertMultiline mappings expr =
    case unFixAST expr of
        (_, VarExpr (VarRef ns n)) | isLocal ns ->
            case Dict.lookup n mappings of
                Just e ->
                    (\(FixAST ((_, ann), x)) -> FixAST ((insertMultiline, ann), x)) $
                    convertFix (\(ann, x) -> ((False, ann), x)) e

                Nothing ->
                    expr

        (ann, AST.Expression.Tuple terms' multiline) ->
            let
                requestedMultiline (C _ (FixAST ((m, _), _))) = m
                newMultiline = multiline || any requestedMultiline terms'
            in
            FixAST $ (,) ann $ AST.Expression.Tuple terms' newMultiline

        -- TODO: handle expanding multiline in contexts other than tuples

        _ -> expr


inlineTypeVars ::
    -- TODO: can Coapplciative annf, Applicative annf be reduced to Functor annf?
    (Coapplicative annf, Applicative annf, Coapplicative annf') =>
    Dict.Map LowercaseIdentifier (FixASTNS Typ annf' ns)
    -> FixASTNS Typ annf ns
    -> FixASTNS Typ annf ns
inlineTypeVars mappings typ =
    case extract $ unFixAST typ of
        TypeVariable ref ->
            case Dict.lookup ref mappings of
                Just replacement -> convertFix (pure . extract) replacement
                Nothing -> typ

        _ -> typ


destructureFirstMatch ::
    Coapplicative annf =>
    C1 before UExpr
    -> [ ( C1 before (FixASTNS Pattern annf (MatchedNamespace [UppercaseIdentifier]))
         , UExpr
         )
       ]
    -> UExpr
    -> UExpr
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
withComments pre e post = FixAST $ (,) FromUpgradeDefinition $ Parens $ C (pre, post) e


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
    C1 before (FixASTNS Pattern annf (MatchedNamespace [UppercaseIdentifier]))
    -> C1 before UExpr
    -> DestructureResult (Dict.Map LowercaseIdentifier UExpr)
destructure pat arg =
    let
        namespaceMatch nsd ns =
            case (nsd, ns) of
                (Unmatched nsd', MatchedImport ns') -> nsd' == ns'
                _ -> nsd == ns
    in
    case (fmap (extract . unFixAST) pat, fmap (extract . unFixAST) arg) of
        -- Parens in expression
        ( _
          , C preArg (AST.Expression.Parens (C (pre, post) inner))
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
          , C preArg (AST.Expression.Unit _)
          )
          ->
            Matches Dict.empty

        -- Literals
        ( C preVar (AST.Pattern.Literal pat)
          , C preArg (AST.Expression.Literal val)
          )
          ->
            -- TODO: handle ints/strings that are equal but have different representations
            if pat == val then
                Matches Dict.empty
            else
                DoesntMatch

        -- Custom type variants with no arguments
        ( C preVar (Data (nsd, name) [])
          , C preArg (VarExpr (TagRef ns tag))
          )
          ->
            if name == tag && namespaceMatch nsd ns then
                Matches Dict.empty
            else
                DoesntMatch

        -- Custom type variants with arguments
        ( C preVar (Data (nsd, name) argVars)
          , C preArg (App (FixAST (_, VarExpr (TagRef ns tag))) argValues _)
          )
          ->
            if name == tag && namespaceMatch nsd ns then
                Dict.unions <$> zipWithM destructure argVars argValues
            else
                DoesntMatch

        -- Custom type variants where pattern and value don't match in having args
        ( C _ (Data _ (_:_)), C _ (VarExpr (TagRef _ _)) ) -> DoesntMatch
        ( C _ (Data _ []), C _ (App (FixAST (_, VarExpr (TagRef _ _))) _ _) ) -> DoesntMatch

        -- Named variable pattern
        ( C preVar (VarPattern name)
          , C preArg arg'
          ) ->
            Matches $ Dict.singleton name (withComments (preVar ++ preArg) (extract arg) [])

        -- `<|` which could be parens in expression
        ( _
          , C preArg (AST.Expression.Binops e (BinopsClause pre (OpRef (SymbolIdentifier "<|")) post arg1 : rest) ml)
          )
          ->
            destructure pat (C preArg $ FixAST $ (,) FromSource $ App e [(C (pre ++ post) $ FixAST $ (,) FromSource $ Binops arg1 rest ml)] (FAJoinFirst JoinAll))

        -- Tuple with two elements (TODO: generalize this for all tuples)
        ( C preVar (AST.Pattern.Tuple varItems)
          , C preArg (AST.Expression.Tuple argItems _)
          ) ->
            case (fmap (fmap (extract . unFixAST)) varItems, argItems) of
                ( [C (preA, postA) (VarPattern nameA), C (preB, postB) (VarPattern nameB)]
                  , [C (preAe, postAe) eA, C (preBe, postBe) eB]
                  ) ->
                    Matches $ Dict.fromList
                        [ (nameA, withComments (preVar ++ preArg) (withComments (preA ++ preAe) eA (postAe ++ postA)) [])
                        , (nameB, withComments (preB ++ preBe) eB (postBe ++ postB))
                        ]

                _ -> CouldMatch

        -- Record destructuring
        ( C preVar (AST.Pattern.Record varFields)
          , C preArg (AST.Expression.Record _ argFields _ _)
          ) ->
            let
                args :: Dict.Map LowercaseIdentifier UExpr
                args =
                    argFields
                        |> fmap extract
                        |> fmap (\(Pair (C _ k) (C _ v) _) -> (k, v))
                        |> Dict.fromList

                fieldMapping :: C2 before after LowercaseIdentifier -> Maybe (LowercaseIdentifier, UExpr)
                fieldMapping (C _ var) =
                    (,) var <$> Dict.lookup var args
            in
            Maybe.fromMaybe DoesntMatch $ fmap (Matches . Dict.fromList) $ sequence $ fmap fieldMapping varFields

        -- `as`
        ( C preVar (AST.Pattern.Alias (C _ p) (C _ varName))
          , _
          ) ->
            fmap Dict.unions $ sequence
                [ destructure (C preVar $ FixAST $ Identity $ VarPattern varName) arg
                , destructure (C [] p) arg
                ]

        -- TODO: handle other patterns

        _ ->
            CouldMatch


simplifyFunctionApplication :: Source -> UExpr -> [C1 before UExpr] -> FunctionApplicationMultiline -> UExpr
simplifyFunctionApplication appSource fn args appMultiline =
    case (unFixAST fn, args) of
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
                            FixAST $ (,) appSource $ App
                                (withComments preBody newBody [])
                                restArgs
                                newMultiline

                        _:_ ->
                            -- we applied this argument; try to apply the next argument
                            simplifyFunctionApplication appSource (FixAST $ (,) lambdaSource $ Lambda restVar preBody newBody multiline) restArgs newMultiline
                _ ->
                    -- failed to destructure the next argument, so stop
                    FixAST $ (,) appSource $ App fn args appMultiline


        (_, []) -> fn

        _ -> FixAST $ (,) appSource $ App fn args appMultiline
