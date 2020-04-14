
module ElmRefactor.Integration.SimplifyTest where

import Elm.Utils ((|>))
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


refactorTest :: String -> [Text] -> [Text] -> [Text] -> TestTree
refactorTest testName inputFile upgradeFile expectedOutputFile =
    testCase testName $
        assertEqual "output should be as expected"
            (Right $ unlines expectedOutputFile)
            (refactor (unlines upgradeFile) (unlines inputFile))

test_tests :: TestTree
test_tests =
    testGroup "simplifying code"
        [ refactorTest "evaluates `==` comparisons"
            [ "import A exposing (a)"
            , "x = A.f \"special\""
            ]
            [ "upgrade_A_f s = s == \"special\"" ]
            [ "module Main exposing (x)"
            , ""
            , ""
            , "x ="
            , "    True"
            ]
        , refactorTest "does not simplify binops from the source file"
            [ "x = \"special\" == \"other\""
            ]
            [ "upgrade_X_x = ()" ]
            [ "module Main exposing (x)"
            , ""
            , ""
            , "x ="
            , "    \"special\" == \"other\""
            ]
        , refactorTest "evaluates `++`"
            [ "import A exposing (a)"
            , "x = A.f [1, 2] [3, 4]"
            ]
            [ "upgrade_A_f a b = a ++ b" ]
            [ "module Main exposing (x)"
            , ""
            , ""
            , "x ="
            , "    [ 1, 2, 3, 4 ]"
            ]
        ]


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
