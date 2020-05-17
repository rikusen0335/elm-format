{-# LANGUAGE DataKinds #-}
module AST.MatchReferencesTest (tests) where

import Elm.Utils ((|>))

import AST.V0_16
import AST.MatchReferences
import AST.Module (ImportMethod(..))
import Data.Functor.Identity
import qualified Data.Indexed as I
import Expect
import ElmFormat.ImportInfo (ImportInfo)
import ElmVersion
import qualified ElmFormat.ImportInfo as ImportInfo
import qualified Parse.Module
import qualified Parse.Parse as Parse
import qualified Reporting.Result as Result
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Map as Dict
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)

tests :: TestTree
tests =
    testGroup "AST.MatchReferences"
    [ testGroup "matchReferences" $
        let
            makeImportInfo :: [(String, List String)] -> [String] -> ImportInfo [UppercaseIdentifier]
            makeImportInfo knownContentsRaw imports =
                let
                    knownContents = knownContentsRaw |> fmap makeKnownContent |> Dict.fromList
                in
                ImportInfo.fromImports (fromMaybe mempty . flip Dict.lookup knownContents)
                (imports |> fmap makeImportMethod |> Dict.fromList)

            makeKnownContent (moduleName, known) =
                ( fmap UppercaseIdentifier $ splitOn "." moduleName
                , fmap (VarName . LowercaseIdentifier) known
                )

            makeImportMethod :: String -> ([UppercaseIdentifier], ImportMethod)
            makeImportMethod importString =
                case Result.toMaybe $ Parse.parse importString (Parse.Module.import' Elm_0_19) of
                    Nothing -> undefined -- Not handled: fix the test input to parse correctly
                    Just (C _ moduleName, importMethod) ->
                        (moduleName, importMethod)

            test ::
                String
                -> [(String, List String)] -- knownContents
                -> [String] -- imports
                -> Ref [UppercaseIdentifier]
                -> Ref (MatchedNamespace [UppercaseIdentifier])
                -> TestTree
            test name knownContents imports sourceAst matchedAst =
                testCase name $
                    matchReferences (makeImportInfo knownContents imports) (I.Fix $ Identity $ VarExpr sourceAst)
                        |> Expect.equals (I.Fix $ Identity $ VarExpr matchedAst)
        in
        [ test "identifies unknown references"
            [] []
            (VarRef [UppercaseIdentifier "A"] (LowercaseIdentifier "a"))
            (VarRef (Unmatched [UppercaseIdentifier "A"]) (LowercaseIdentifier "a"))
        , test "matches references from an import"
            []
            [ "import A" ]
            (VarRef [UppercaseIdentifier "A"] (LowercaseIdentifier "a"))
            (VarRef (MatchedImport True [UppercaseIdentifier "A"]) (LowercaseIdentifier "a"))
        , test "matches reference to a known value via exposing(..)"
            [ ("Html", ["div"]) ]
            [ "import Html exposing (..)" ]
            (VarRef [] (LowercaseIdentifier "div"))
            (VarRef (MatchedImport False [UppercaseIdentifier "Html"]) (LowercaseIdentifier "div"))
        ]
    ]
