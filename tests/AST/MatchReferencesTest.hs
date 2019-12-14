module AST.MatchReferencesTest (tests) where

import Elm.Utils ((|>))

import AST.V0_16
import AST.Expression (Expression(..))
import AST.MatchReferences
import AST.Module (ImportMethod(..), DetailedListing(..))
import AST.Structure
import AST.Variable (Listing(..), Ref(..))
import Data.Functor.Identity
import Expect
import ElmFormat.ImportInfo (ImportInfo)
import qualified ElmFormat.ImportInfo as ImportInfo
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Map as Dict
import qualified Data.Set as Set
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
                , DetailedListing
                    (known |> fmap (\l -> ( LowercaseIdentifier l, C ([], []) () )) |> Dict.fromList)
                    mempty -- TODO
                    mempty -- TODO
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
                -> ASTNS Expression Identity [UppercaseIdentifier]
                -> ASTNS Expression Identity (MatchedNamespace [UppercaseIdentifier])
                -> TestTree
            test name imports sourceAst matchedAst =
                testCase name $
                    matchReferences (makeImportInfo imports) sourceAst
                        |> Expect.equals matchedAst
        in
        [ test "identifies unknown references" []
            (VarExpr (VarRef [UppercaseIdentifier "A"] (LowercaseIdentifier "a")))
            (VarExpr (VarRef (Unmatched [UppercaseIdentifier "A"]) (LowercaseIdentifier "a")))
        , test "matches references from an import"
            [ ("A", [], Nothing, closedListing) ]
            (VarExpr (VarRef [UppercaseIdentifier "A"] (LowercaseIdentifier "a")))
            (VarExpr (VarRef (MatchedImport [UppercaseIdentifier "A"]) (LowercaseIdentifier "a")))
        , test "matches reference to a known value via exposing(..)"
            [ ("Html", [ "div" ], Nothing, openListing) ]
            (VarExpr (VarRef [] (LowercaseIdentifier "div")))
            (VarExpr (VarRef (MatchedImport [UppercaseIdentifier "Html"]) (LowercaseIdentifier "div")))
        ]
    ]
