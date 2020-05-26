module ElmCompilerAdapter (knownContents) where

{-| This module converts between elm/compiler modules and elm-format-lib modules.
-}

import Elm.Utils ((|>))

import AST.V0_16
import qualified Data.List as List
import qualified Data.Map as Map
import qualified ElmFormat.KnownContents as KnownContents
import ElmFormat.KnownContents (KnownContents)
import qualified ElmBuilder.Elm.Details as Details
import qualified ElmCompiler.Data.Utf8 as Utf8
import qualified ElmCompiler.Elm.Interface as Interface
import qualified ElmCompiler.Elm.ModuleName as ModuleName


knownContents :: Details.Interfaces -> KnownContents
knownContents interfaces =
    KnownContents.fromFunction $
        \ns ->
            let
                interfacesByModuleName =
                    -- TODO: is it possible for multiple interfaces to have the same module name?
                    Map.mapKeys ModuleName._module interfaces

                elmCompilerNs =
                    Utf8.fromChars $ List.intercalate "." $ fmap (\(UppercaseIdentifier ns') -> ns') ns
            in
            localNames <$> Map.lookup elmCompilerNs interfacesByModuleName


localNames :: Interface.DependencyInterface -> [LocalName]
localNames depInterface =
    case depInterface of
        Interface.Public interface ->
            mconcat
                [ Interface._values interface
                    |> Map.keys
                    |> fmap (VarName . LowercaseIdentifier .  Utf8.toChars)
                , Interface._unions interface
                    |> Map.keys
                    |> fmap (TypeName . UppercaseIdentifier .  Utf8.toChars)
                , Interface._aliases interface
                    |> Map.keys
                    |> fmap (TypeName . UppercaseIdentifier .  Utf8.toChars)
                ]
            -- TODO: include custom type variants
            -- TODO: include symbol operators

        Interface.Private _ _ _ ->
            -- TODO: when are Private interfaces used?
            error "TODO: Interface.Private"
