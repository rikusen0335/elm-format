module ElmRefactor.CliTest (tests) where

import Elm.Utils ((|>))
import Test.Tasty

import CommandLine.TestWorld (TestWorld, run, goldenExitStdout)
import qualified CommandLine.TestWorld as TestWorld
import qualified ElmRefactor.Cli as ElmRefactor


tests :: TestTree
tests =
    testGroup "CLI"
        [ goldenExitStdout "usage instructions" 0 "test/usage.stdout" $ world
            |> run "elm-refactor" [ "--help" ]
        ]


world :: TestWorld
world =
    TestWorld.init
        |> TestWorld.installProgram "elm-refactor" ElmRefactor.main
