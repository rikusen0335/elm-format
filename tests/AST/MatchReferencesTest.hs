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
                ImportInfo.fromImports
                    (flip Dict.lookup knownContents)
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

            makeLetDeclaration name =
                I.Fix $ Identity $
                LetDefinition
                    (I.Fix $ Identity $ VarPattern $ LowercaseIdentifier name)
                    [] []
                    (I.Fix $ Identity $ Unit [])

            test ::
                String
                -> [(String, List String)] -- knownContents
                -> List String -- imports
                -> List String -- locals
                -> Ref [String]
                -> Ref (MatchedNamespace [String])
                -> TestTree
            test name knownContents imports locals sourceAst' matchedAst' =
                let
                    sourceAst = fmap (fmap UppercaseIdentifier) sourceAst'
                    matchedAst = fmap (fmap $ fmap UppercaseIdentifier) matchedAst'
                    wrapExpr r =
                        case locals of
                            [] ->
                                -- no locals to define, so just make a var expression
                                I.Fix $ Identity $ VarExpr r
                            _ ->
                                -- define the provided locals in a let block
                                I.Fix $ Identity $
                                Let
                                    (fmap makeLetDeclaration locals)
                                    []
                                    (I.Fix $ Identity $ VarExpr r)
                in
                testCase name $
                    matchReferences (makeImportInfo knownContents imports) (wrapExpr sourceAst)
                        |> Expect.equals (wrapExpr matchedAst)
        in
        [ test "identifies unknown references"
            [] [] []
            (VarRef ["A"] (LowercaseIdentifier "a"))
            (VarRef (Unmatched ["A"]) (LowercaseIdentifier "a"))
        , test "matches references from an import"
            []
            [ "import A" ]
            []
            (VarRef ["A"] (LowercaseIdentifier "a"))
            (VarRef (MatchedImport True ["A"]) (LowercaseIdentifier "a"))
        , test "matches reference to a known value via exposing(..)"
            [ ("Html", ["div"]) ]
            [ "import Html exposing (..)" ]
            []
            (VarRef [] (LowercaseIdentifier "div"))
            (VarRef (MatchedImport False ["Html"]) (LowercaseIdentifier "div"))
        , test "determines references to local variables"
            [] []
            [ "a" ]
            (VarRef [] (LowercaseIdentifier "a"))
            (VarRef Local (LowercaseIdentifier "a"))
        , test "determines unqualified references that are unmatched"
            [] [] []
            (VarRef [] (LowercaseIdentifier "a"))
            (VarRef (UnmatchedUnqualified []) (LowercaseIdentifier "a"))
        , test "determines when an unqualified reference might match"
            []
            [ "import Test exposing (..)" ]
            []
            (VarRef [] (LowercaseIdentifier "describe"))
            (VarRef (UnmatchedUnqualified [["Test"]]) (LowercaseIdentifier "describe"))
        ]
    ]
