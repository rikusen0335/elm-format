{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
module ElmCompiler.Compile
  ( Artifacts(..)
  , compile
  )
  where


import qualified Data.Map as Map
import qualified ElmCompiler.Data.Name as Name

import qualified ElmCompiler.AST.Source as Src
import qualified ElmCompiler.AST.Canonical as Can
import qualified ElmCompiler.AST.Optimized as Opt
import qualified ElmCompiler.Canonicalize.Module as Canonicalize
import qualified ElmCompiler.Elm.Interface as I
import qualified ElmCompiler.Elm.ModuleName as ModuleName
import qualified ElmCompiler.Elm.Package as Pkg
import qualified ElmCompiler.Nitpick.PatternMatches as PatternMatches
import qualified ElmCompiler.Optimize.Module as Optimize
import qualified ElmCompiler.Reporting.Error as E
import qualified ElmCompiler.Reporting.Result as R
import qualified ElmCompiler.Reporting.Render.Type.Localizer as Localizer
import qualified ElmCompiler.Type.Constrain.Module as Type
import qualified ElmCompiler.Type.Solve as Type

import System.IO.Unsafe (unsafePerformIO)



-- COMPILE


data Artifacts =
  Artifacts
    { _modul :: Can.Module
    , _types :: Map.Map Name.Name Can.Annotation
    , _graph :: Opt.LocalGraph
    }


compile :: Pkg.Name -> Map.Map ModuleName.Raw I.Interface -> Src.Module -> Either E.Error Artifacts
compile pkg ifaces modul =
  do  canonical   <- canonicalize pkg ifaces modul
      annotations <- typeCheck modul canonical
      ()          <- nitpick canonical
      objects     <- optimize modul annotations canonical
      return (Artifacts canonical annotations objects)



-- PHASES


canonicalize :: Pkg.Name -> Map.Map ModuleName.Raw I.Interface -> Src.Module -> Either E.Error Can.Module
canonicalize pkg ifaces modul =
  case snd $ R.run $ Canonicalize.canonicalize pkg ifaces modul of
    Right canonical ->
      Right canonical

    Left errors ->
      Left $ E.BadNames errors


typeCheck :: Src.Module -> Can.Module -> Either E.Error (Map.Map Name.Name Can.Annotation)
typeCheck modul canonical =
  case unsafePerformIO (Type.run =<< Type.constrain canonical) of
    Right annotations ->
      Right annotations

    Left errors ->
      Left (E.BadTypes (Localizer.fromModule modul) errors)


nitpick :: Can.Module -> Either E.Error ()
nitpick canonical =
  case PatternMatches.check canonical of
    Right () ->
      Right ()

    Left errors ->
      Left (E.BadPatterns errors)


optimize :: Src.Module -> Map.Map Name.Name Can.Annotation -> Can.Module -> Either E.Error Opt.LocalGraph
optimize modul annotations canonical =
  case snd $ R.run $ Optimize.optimize annotations canonical of
    Right localGraph ->
      Right localGraph

    Left errors ->
      Left (E.BadMains (Localizer.fromModule modul) errors)
