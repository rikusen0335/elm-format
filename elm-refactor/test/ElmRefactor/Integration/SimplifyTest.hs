module ElmRefactor.Integration.SimplifyTest where

import Prelude ()
import ElmRefactor.Prelude

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Bifunctor
import Data.Coapplicative
import qualified Data.Indexed as I
import qualified Data.Text as Text
import qualified ElmFormat.Parse as Parse
import qualified ElmFormat.Render.Text as Render
import qualified ElmFormat.Upgrade_0_19
import qualified ElmVersion


test_tests :: TestTree
test_tests =
    testGroup "simplifying code"
        [ refactorExpressionTest "evaluates `==` comparisons"
            [ "A.f \"special\"" ]
            [ "upgrade_A_f s = s == \"special\"" ]
            [ "True" ]
        , refactorExpressionTest "does not simplify binops from the source file"
            [ "\"special\" == \"other\"" ]
            [ "upgrade_X_x = ()" ]
            [ "\"special\" == \"other\"" ]
        , refactorExpressionTest "evaluates `++`"
            [ "A.f [1, 2] [3, 4]" ]
            [ "upgrade_A_f a b = a ++ b" ]
            [ "[ 1, 2, 3, 4 ]" ]
        , testGroup "List.filterMap identity"
            [ refactorExpressionTest "removes Nothing values"
                [ "A.f [ Nothing, Nothing ]"]
                [ "upgrade_A_f list = List.filterMap identity list"]
                [ "[]" ]
            , refactorExpressionTest "simplifies with (Just x) values"
                [ "A.f [ Just () ]"]
                [ "upgrade_A_f list = List.filterMap identity list"]
                [ "[ () ]" ]
            , refactorExpressionTest "does not break code when it can't simplify"
                [ "\\x -> A.f [ x ]"]
                [ "upgrade_A_f list = List.filterMap identity list"]
                [ "\\x -> List.filterMap identity [ x ]" ]
            , refactorExpressionTest "simplifies with (Just <| x) values"
                [ "A.f [ Just <| () ]"]
                [ "upgrade_A_f list = List.filterMap identity list"]
                [ "[ () ]" ]
            , refactorExpressionTest "simplifies with (x |> Just) values"
                [ "A.f [ always () |> Just ]"]
                [ "upgrade_A_f list = List.filterMap identity list"]
                [ "[ always () ]" ]
            ]
        , testGroup "Maybe.map"
            [ refactorExpressionTest "simplifies Nothing"
                [ "A.f Nothing"]
                [ "upgrade_A_f m = Maybe.map f m"]
                [ "Nothing" ]
            , refactorExpressionTest "simplifies Just"
                [ "A.f (Just x)"]
                [ "upgrade_A_f m = Maybe.map f m"]
                [ "Just (f x)" ]
            ]
        ]


{-| This just makes tests more concise by building the input and expected output files from the given expressions. -}
refactorExpressionTest :: String -> [Text] -> [Text] -> [Text] -> TestTree
refactorExpressionTest testName inputExpression upgradeFile expectedOutputExpression =
    refactorTest testName
        (fileWithExpression inputExpression)
        upgradeFile
        (fileWithExpression expectedOutputExpression)
    where
        fileWithExpression e =
          [ "module Main exposing (..)"
          , ""
          , "import A"
          , ""
          , ""
          , "keepAImport ="
          , "    A.keep"
          , ""
          , ""
          , "x ="
          ]
          ++
          fmap ((<>) "    ") e


refactorTest :: String -> [Text] -> [Text] -> [Text] -> TestTree
refactorTest testName inputFile upgradeFile expectedOutputFile =
    testCase testName $
        assertEqual "output should be as expected"
            (Right $ unlines expectedOutputFile)
            (refactor (unlines upgradeFile) (unlines inputFile))


refactor :: Text -> Text -> Either [String] Text.Text
refactor upgradeFile input =
    do
        upgradeDefinition <-
            upgradeFile
                |> ElmFormat.Upgrade_0_19.parseUpgradeDefinition
                |> Data.Bifunctor.first (const ["unable to parse upgrade definition"])
        input
            |> Parse.parse ElmVersion.Elm_0_19
            |> Parse.toEither
            |> fmap (fmap $ I.convert (Identity . extract))
            |> fmap (ElmFormat.Upgrade_0_19.transformModule upgradeDefinition)
            |> fmap (Render.render ElmVersion.Elm_0_19)
            |> Data.Bifunctor.first (fmap show)
