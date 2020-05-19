module ElmRefactor.Integration.NormalizingReferencesTest where

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


refactorTest :: String -> [Text] -> [Text] -> [Text] -> TestTree
refactorTest testName inputFile upgradeFile expectedOutputFile =
    testCase testName $
        assertEqual "output should be as expected"
            (Right $ unlines expectedOutputFile)
            (refactor (unlines upgradeFile) (unlines inputFile))

test_tests :: TestTree
test_tests =
    testGroup "normalizing references"
        [ refactorTest "exposing (..) in an import prevents unqualifying references to things explicitly exposed"
            [ "import A exposing (a)"
            , "import Other exposing (..)"
            , "x = ( A.a, Other.b )"
            ]
            [ "upgrade_X_x = ()" ]
            [ "module Main exposing (x)"
            , ""
            , "import A exposing (a)"
            , "import Other exposing (..)"
            , ""
            , ""
            , "x ="
            , "    ( A.a, Other.b )"
            ]
        , refactorTest "exposing (..) in an import does not qualify references that were previously unqualified"
            [ "import A exposing (a)"
            , "import Other exposing (..)"
            , "x = ( a, Other.b )"
            ]
            [ "upgrade_X_x = ()" ]
            [ "module Main exposing (x)"
            , ""
            , "import A exposing (a)"
            , "import Other exposing (..)"
            , ""
            , ""
            , "x ="
            , "    ( a, Other.b )"
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
