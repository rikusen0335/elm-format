module ElmRefactor.Integration.UpgradeTest where

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
    testGroup "applying upgrade definitions"
        [ refactorTest "keeps import from original"
            [ "import A"
            , "x = A.a"
            ]
            [ "upgrade_Z_z = ()" ]
            [ "module Main exposing (x)"
            , ""
            , "import A"
            , ""
            , ""
            , "x ="
            , "    A.a"
            ]
        , refactorTest "removes unused imports that were the subject of the upgrade"
            [ "import A"
            , "x = ()"
            ]
            [ "upgrade_A_a = ()" ]
            [ "module Main exposing (x)"
            , ""
            , ""
            , "x ="
            , "    ()"
            ]
        , refactorTest "does not remove unused imports from the original"
            [ "import A"
            , "x = ()"
            ]
            [ "upgrade_Z_z = ()" ]
            [ "module Main exposing (x)"
            , ""
            , "import A"
            , ""
            , ""
            , "x ="
            , "    ()"
            ]
        , refactorTest "adds import from upgrade if it is used"
            [ "import Z"
            , "x = Z.z"
            ]
            [ "import B"
            , "upgrade_Z_z = B.b"
            ]
            [ "module Main exposing (x)"
            , ""
            , "import B"
            , ""
            , ""
            , "x ="
            , "    B.b"
            ]
        , refactorTest "does not add import from upgrade if it is not used"
            [ "x = ()"
            ]
            [ "import B"
            , "upgrade_Z_z = B.b"
            ]
            [ "module Main exposing (x)"
            , ""
            , ""
            , "x ="
            , "    ()"
            ]
        ]


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
