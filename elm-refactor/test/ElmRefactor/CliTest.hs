module ElmRefactor.CliTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import CommandLine.TestWorld hiding (uploadFile)
import qualified CommandLine.TestWorld as TestWorld
import qualified CommandLine.World as World
import qualified ElmRefactor.Cli as ElmRefactor


tests :: TestTree
tests =
    testGroup "CLI"
        [ goldenExitStdout "usage instructions" 0 "test/usage.stdout" $ world
            |> run "elm-refactor" [ "--help" ]
        , testCase "--upgrade" $ world
            |> uploadFile "Main.elm"
                [ "module Main exposing (..)"
                , "import Data.Maybe"
                , "x = Data.Maybe.maybe 0 increment"
                ]
            |> uploadFile "upgrade.elm"
                [ "import Maybe"
                , "upgrade_Data_Maybe_maybe onNothing onJust maybe = maybe |> Maybe.map onJust |> Maybe.withDefault onNothing"
                ]
            |> run "elm-refactor" [ "--upgrade", "upgrade.elm", "Main.elm" ]
            |> assertFile "Main.elm"
                [ "module Main exposing (..)"
                , ""
                , "import Maybe"
                , ""
                , ""
                , "x ="
                , "    \\maybe -> maybe |> Maybe.map increment |> Maybe.withDefault 0"
                ]
        , testCase "transforms all files in a directory" $ world
            |> uploadFile "src/Main.elm"
                [ "module Main exposing (..)"
                , "import Data.Maybe"
                , "x = Data.Maybe.maybe 0 increment"
                ]
            |> uploadFile "upgrade.elm"
                [ "import Maybe"
                , "upgrade_Data_Maybe_maybe onNothing onJust maybe = maybe |> Maybe.map onJust |> Maybe.withDefault onNothing"
                ]
            |> run "elm-refactor" [ "--upgrade", "upgrade.elm", "src" ]
            |> assertFile "src/Main.elm"
                [ "module Main exposing (..)"
                , ""
                , "import Maybe"
                , ""
                , ""
                , "x ="
                , "    \\maybe -> maybe |> Maybe.map increment |> Maybe.withDefault 0"
                ]
        ]


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
