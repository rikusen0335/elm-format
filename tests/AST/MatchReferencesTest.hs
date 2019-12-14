module AST.MatchReferencesTest (tests) where

import Elm.Utils ((|>))

import AST.V0_16
import AST.Expression (Expression(..))
import AST.MatchReferences
import AST.Module (ImportMethod(..))
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

tests :: TestTree
tests =
    testGroup "AST.MatchReferences"
    [ testGroup "matchReferences" $
        let
            makeImportInfo :: [(String, List todo, Maybe String, List todo)] -> ImportInfo [UppercaseIdentifier]
            makeImportInfo imports =
                ImportInfo.fromImports (\_ -> undefined)
                (imports |> fmap makeImportMethod |> Dict.fromList)

            makeImportMethod :: (String, knownContents, Maybe String, List exposing) -> ([UppercaseIdentifier], ImportMethod)
            makeImportMethod (moduleName, _, as, exposing) =
                ( fmap UppercaseIdentifier $ splitOn "." moduleName
                , ImportMethod
                    (fmap (C ([], []) . UppercaseIdentifier) as)
                    (C ([], []) ClosedListing) -- TODO make listing from `exposing`
                )

            test ::
                String
                -> [(String, List todo, Maybe String, List todo)]
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
            [ ("A", [], Nothing, []) ]
            (VarExpr (VarRef [UppercaseIdentifier "A"] (LowercaseIdentifier "a")))
            (VarExpr (VarRef (MatchedImport [UppercaseIdentifier "A"]) (LowercaseIdentifier "a")))
        ]
    ]
