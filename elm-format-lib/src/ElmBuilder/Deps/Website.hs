module ElmBuilder.Deps.Website
  ( domain
  , route
  , metadata
  )
  where


import qualified ElmCompiler.Elm.Package as Pkg
import qualified ElmCompiler.Elm.Version as V
import qualified ElmBuilder.Http as Http


domain :: String
domain =
  "https://package.elm-lang.org"


route :: String -> [(String,String)] -> String
route path params =
  Http.toUrl (domain ++ path) params


metadata :: Pkg.Name -> V.Version -> String -> String
metadata name version file =
  domain ++ "/packages/" ++ Pkg.toUrl name ++ "/" ++ V.toChars version ++ "/" ++ file
