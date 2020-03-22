module ElmRefactor.Integration.NormalizingReferencesTest (tests) where

import Elm.Utils ((|>))
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Bifunctor
import Data.Coapplicative
import Data.Functor.Identity
import qualified Data.Indexed as I
import qualified Data.Text as Text
import qualified ElmFormat.Parse as Parse
import qualified ElmFormat.Render.Text as Render
import qualified ElmFormat.Upgrade_0_19
import qualified ElmVersion


refactorTest :: String -> [String] -> [String] -> [String] -> TestTree
refactorTest testName inputFile upgradeFile expectedOutputFile =
    testCase testName $
        assertEqual "output should be as expected"
            (Right $ Text.pack (unlines expectedOutputFile))
            (refactor (unlines upgradeFile) (unlines inputFile))

tests :: TestTree
tests =
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
        ]


refactor :: String -> String -> Either [String] Text.Text
refactor upgradeFile input =
    do
        upgradeDefinition <-
            upgradeFile
                |> Text.pack
                |> ElmFormat.Upgrade_0_19.parseUpgradeDefinition
                |> Data.Bifunctor.first (const ["unable to parse upgrade definition"])
        input
            |> Text.pack
            |> Parse.parse ElmVersion.Elm_0_19
            |> Parse.toEither
            |> fmap (fmap $ I.convert (Identity . extract))
            |> fmap (ElmFormat.Upgrade_0_19.transformModule upgradeDefinition)
            |> fmap (Render.render ElmVersion.Elm_0_19)
            |> Data.Bifunctor.first (fmap show)
