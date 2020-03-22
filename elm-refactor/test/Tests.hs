module Main where

import Test.Tasty

import qualified ElmRefactor.Integration.NormalizingReferencesTest


main :: IO ()
main =
    do
        defaultMain $ testGroup "elm-refactor" $
            [ ElmRefactor.Integration.NormalizingReferencesTest.tests
            ]
