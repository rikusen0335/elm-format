module ElmFormat.ImportInfo (ImportInfo(..), fromModule, fromImports) where

import AST.V0_16
import AST.Listing (Listing(..))
import Elm.Utils ((|>))

import AST.Module (Module)
import qualified AST.Module
import Data.Coapplicative
import qualified Data.Bimap as Bimap
import qualified Data.Map.Strict as Dict
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

data ImportInfo ns =
    ImportInfo
        { _exposed :: Dict.Map LocalName ns
        , _aliases :: Bimap.Bimap ns ns
        , _directImports :: Set.Set ns
        , _ambiguous :: Dict.Map LocalName [ns]
        , _unresolvedExposingAll :: Bool -- True if there is an exposing(..) and we didn't know the module contents
        }
    deriving Show


fromModule ::
    ([UppercaseIdentifier] -> [LocalName])
    -> Module [UppercaseIdentifier] decl
    -> ImportInfo [UppercaseIdentifier]
fromModule knownModuleContents modu =
    fromImports knownModuleContents (fmap extract $ extract $ AST.Module.imports $ modu)


fromImports ::
    ([UppercaseIdentifier] -> [LocalName])
    -> Dict.Map [UppercaseIdentifier] AST.Module.ImportMethod
    -> ImportInfo [UppercaseIdentifier]
fromImports knownModuleContents imports =
    let
        -- these are things we know will get exposed for certain modules when we see "exposing (..)"
        -- only things that are currently useful for Elm 0.19 upgrade are included
        moduleContents :: [UppercaseIdentifier] -> [LocalName]
        moduleContents moduleName =
            case (\(UppercaseIdentifier x) -> x) <$> moduleName of
                ["Html", "Attributes"] ->
                    [ VarName $ LowercaseIdentifier "style"
                    ]
                _ -> knownModuleContents moduleName

        getExposed moduleName (AST.Module.ImportMethod _ (C _ listing)) =
            Dict.fromList $ fmap (flip (,) moduleName) $
            case listing of
                ClosedListing -> []
                OpenListing _ ->
                    moduleContents moduleName
                ExplicitListing details _ ->
                    -- TODO: exposing (Type(..)) should pull in variant names from knownModuleContents, though this should also be a warning because we can't know for sure which of those are for this type
                    (fmap VarName $ Dict.keys $ AST.Module.values details)
                    <> (fmap TypeName $ Dict.keys $ AST.Module.types details)

        exposed =
            -- TODO: mark ambiguous names if multiple modules expose them
            Dict.foldlWithKey (\a k v -> Dict.union a $ getExposed k v) mempty imports

        aliases =
            let
                getAlias importMethod =
                    case AST.Module.alias importMethod of
                        Just (C _ alias) ->
                            Just [alias]

                        Nothing -> Nothing

                liftMaybe :: (a, Maybe b) -> Maybe (a, b)
                liftMaybe (_, Nothing) = Nothing
                liftMaybe (a, Just b) = Just (a, b)
            in
            Dict.toList imports
                |> fmap (fmap getAlias)
                |> Maybe.mapMaybe liftMaybe
                |> fmap (\(a, b) -> (b, a))
                |> Bimap.fromList

        noAlias importMethod =
            case AST.Module.alias importMethod of
                Just _ -> False
                Nothing -> True

        directs =
            Set.union
                (Set.singleton [UppercaseIdentifier "Basics"])
                (Dict.keysSet $ Dict.filter noAlias imports)

        ambiguous = Dict.empty

        exposesAll (AST.Module.ImportMethod _ (C _ listing)) =
            case listing of
                ExplicitListing _ _ -> False
                OpenListing _ -> True
                ClosedListing -> False

        unresolvedExposingAll =
            any exposesAll imports
    in
    ImportInfo exposed aliases directs ambiguous unresolvedExposingAll
