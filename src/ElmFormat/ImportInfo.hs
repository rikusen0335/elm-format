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
        }
    deriving Show


fromModule ::
    ([UppercaseIdentifier] -> AST.Module.DetailedListing)
    -> Module [UppercaseIdentifier] decl
    -> ImportInfo [UppercaseIdentifier]
fromModule knownModuleContents modu =
    fromImports knownModuleContents (fmap extract $ extract $ AST.Module.imports $ modu)


fromImports ::
    ([UppercaseIdentifier] -> AST.Module.DetailedListing)
    -> Dict.Map [UppercaseIdentifier] AST.Module.ImportMethod
    -> ImportInfo [UppercaseIdentifier]
fromImports knownModuleContents imports =
    let
        -- these are things we know will get exposed for certain modules when we see "exposing (..)"
        -- only things that are currently useful for Elm 0.19 upgrade are included
        moduleContents :: [UppercaseIdentifier] -> AST.Module.DetailedListing
        moduleContents moduleName =
            case (\(UppercaseIdentifier x) -> x) <$> moduleName of
                ["Html", "Attributes"] ->
                    AST.Module.DetailedListing
                        (Dict.fromList $ fmap (\x -> (LowercaseIdentifier x, C ([], []) ())) $
                            [ "style"
                            ]
                        )
                        mempty
                        mempty
                _ -> knownModuleContents moduleName

        getExposed moduleName (AST.Module.ImportMethod _ (C _ listing)) =
            Dict.fromList $ fmap (flip (,) moduleName) $
            case listing of
                ClosedListing -> []
                OpenListing _ ->
                    (fmap VarName $ Dict.keys $ AST.Module.values $ moduleContents moduleName)
                    <> (fmap TypeName $ Dict.keys $ AST.Module.types $ moduleContents moduleName)
                ExplicitListing details _ ->
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
    in
    ImportInfo exposed aliases directs ambiguous
