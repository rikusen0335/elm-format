{-# LANGUAGE DataKinds #-}
module AST.MatchReferencesTest (tests) where

import Elm.Utils ((|>))

import AST.V0_16
import AST.MatchReferences
import AST.Module (ImportMethod(..), DetailedListing(..))
import AST.Listing (Listing(..))
import Data.Functor.Identity
import qualified Data.Indexed as I
import Expect
import ElmFormat.ImportInfo (ImportInfo)
import qualified ElmFormat.ImportInfo as ImportInfo
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
            makeImportInfo :: [(String, List String, Maybe String, Listing DetailedListing)] -> ImportInfo [UppercaseIdentifier]
            makeImportInfo imports =
                let
                    knownContents = imports |> fmap makeKnownContent |> Dict.fromList
                in
                ImportInfo.fromImports (fromMaybe mempty . flip Dict.lookup knownContents)
                (imports |> fmap makeImportMethod |> Dict.fromList)

            makeKnownContent (moduleName, known, _, _) =
                ( fmap UppercaseIdentifier $ splitOn "." moduleName
                , fmap (VarName . LowercaseIdentifier) known
                )

            makeImportMethod :: (String, knownContents, Maybe String, Listing DetailedListing) -> ([UppercaseIdentifier], ImportMethod)
            makeImportMethod (moduleName, _, as, exposing) =
                ( fmap UppercaseIdentifier $ splitOn "." moduleName
                , ImportMethod
                    (fmap (C ([], []) . UppercaseIdentifier) as)
                    (C ([], []) exposing)
                )

            closedListing = ClosedListing
            openListing = OpenListing (C ([], []) ())

            test ::
                String
                -> [(String, List String, Maybe String, Listing DetailedListing)]
                -> Ref [UppercaseIdentifier]
                -> Ref (MatchedNamespace [UppercaseIdentifier])
                -> TestTree
            test name imports sourceAst matchedAst =
                testCase name $
                    matchReferences (makeImportInfo imports) (I.Fix $ Identity $ VarExpr sourceAst)
                        |> Expect.equals (I.Fix $ Identity $ VarExpr matchedAst)
        in
        [ test "identifies unknown references" []
            (VarRef [UppercaseIdentifier "A"] (LowercaseIdentifier "a"))
            (VarRef (Unmatched [UppercaseIdentifier "A"]) (LowercaseIdentifier "a"))
        , test "matches references from an import"
            [ ("A", [], Nothing, closedListing) ]
            (VarRef [UppercaseIdentifier "A"] (LowercaseIdentifier "a"))
            (VarRef (MatchedImport [UppercaseIdentifier "A"]) (LowercaseIdentifier "a"))
        , test "matches reference to a known value via exposing(..)"
            [ ("Html", [ "div" ], Nothing, openListing) ]
            (VarRef [] (LowercaseIdentifier "div"))
            (VarRef (MatchedImport [UppercaseIdentifier "Html"]) (LowercaseIdentifier "div"))
        ]
    ]
