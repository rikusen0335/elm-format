module ElmRefactor.Integration.ApplyImportsTest where

import Elm.Utils ((|>))
import Test.Tasty
import Test.Tasty.HUnit

import CommandLine.TestWorld (TestWorld, run)
import qualified CommandLine.TestWorld as TestWorld
import qualified CommandLine.World as World
import qualified Data.Bifunctor
import Data.Coapplicative
import qualified Data.Indexed as I
import qualified Data.Text as Text
import qualified ElmFormat.Parse as Parse
import qualified ElmFormat.Render.Text as Render
import qualified ElmFormat.Upgrade_0_19
import qualified ElmRefactor.Cli as ElmRefactor
import qualified ElmVersion


test_tests :: TestTree
test_tests =
    testGroup "--import"
        [ testCase "adding an alias" $ world
            |> uploadFile "Main.elm"
                [ "module Main exposing (..)"
                , "import Html.Attributes"
                , "x = Html.Attributes.style \"color\" \"blue\""
                ]
            |> run "elm-refactor" [ "--import", "Html.Attributes as Attr", "--yes", "Main.elm" ]
            |> assertFile "Main.elm"
                [ "module Main exposing (..)"
                , ""
                , "import Html.Attributes as Attr"
                , ""
                , ""
                , "x ="
                , "    Attr.style \"color\" \"blue\""
                ]
        , testCase "don't remove imports referenced via 'exposing (..)'" $ world
            |> uploadFile "Tests.elm"
                [ "module Tests exposing (all)"
                , "import Test exposing (..)"
                , "all : Test"
                , "all = describe \"example tests\" []"
                ]
            |> run "elm-refactor" [ "--import", "Html.Attributes as Attr", "--yes", "Tests.elm" ]
            |> assertFile "Tests.elm"
                [ "module Tests exposing (all)"
                , ""
                , "import Test exposing (..)"
                , ""
                , ""
                , "all : Test"
                , "all ="
                , "    describe \"example tests\" []"
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


uploadFile :: FilePath -> [Text] -> TestWorld -> TestWorld
uploadFile filename content =
    TestWorld.uploadFile filename (unlines content)


assertFile :: FilePath -> [Text] -> TestWorld -> Assertion
assertFile filename expectedContent =
    TestWorld.eval (World.readUtf8File filename)
        >> assertEqual ("assertFile " ++ filename) (unlines expectedContent)


world :: TestWorld
world =
    TestWorld.init
        |> TestWorld.installProgram "elm-refactor" ElmRefactor.main
