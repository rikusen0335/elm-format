module ElmCompilerAdapterTest where

import AST.V0_16
import qualified Data.Map as Map
import Elm.Utils ((|>))
import qualified ElmCompilerAdapter
import qualified ElmCompiler.Data.Name as Name
import qualified ElmCompiler.Data.Utf8 as Utf8
import qualified ElmCompiler.Elm.Interface as Interface
import ElmCompiler.Elm.Interface (Interface(..))
import qualified ElmCompiler.Elm.ModuleName as ModuleName
import qualified ElmCompiler.Elm.Package as Pkg
import qualified ElmFormat.KnownContents as KnownContents
import Test.Tasty.Hspec


spec_spec :: Spec
spec_spec =
    describe "ElmCompilerAdapter" $ do
        describe "knownContents" $ do
            it "includes values" $ do
                Map.fromList
                    [ ( canonical (pkg "elm" "time") "Time"
                      , Interface.public $ Interface (pkg "elm" "time")
                            (Map.singleton "now" undefined)
                            mempty
                            mempty
                            mempty
                      )
                    ]
                    |> ElmCompilerAdapter.knownContents
                    |> KnownContents.get [ UppercaseIdentifier "Time" ]
                    |> flip shouldBe (Just [ VarName $ LowercaseIdentifier "now" ])


canonical :: Pkg.Name -> Name.Name -> ModuleName.Canonical
canonical packageName moduleName =
    ModuleName.Canonical packageName moduleName


pkg :: String -> String -> Pkg.Name
pkg userName projectName =
    (Pkg.Name (Utf8.fromChars userName) (Utf8.fromChars projectName))
