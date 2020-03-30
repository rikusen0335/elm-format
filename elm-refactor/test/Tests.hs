module Main where

import Test.Tasty

import qualified ElmRefactor.CliTest
import qualified ElmRefactor.Integration.NormalizingReferencesTest


main :: IO ()
main =
    do
        defaultMain $ testGroup "elm-refactor" $
            [ ElmRefactor.CliTest.tests
            , ElmRefactor.Integration.NormalizingReferencesTest.tests
            ]
