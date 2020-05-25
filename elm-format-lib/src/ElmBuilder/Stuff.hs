{-# OPTIONS_GHC -Wall #-}
module ElmBuilder.Stuff
  ( details
  , interfaces
  , objects
  , prepublishDir
  , elmi
  , elmo
  , temp
  , findRoot
  , withRootLock
  , withRegistryLock
  , PackageCache
  , getPackageCache
  , registry
  , package
  , getReplCache
  , getElmHome
  )
  where


import qualified System.Directory as Dir
import qualified System.FilePath as FP
import System.FilePath ((</>), (<.>))

import qualified ElmCompiler.Elm.ModuleName as ModuleName
import qualified ElmCompiler.Elm.Package as Pkg
import qualified ElmCompiler.Elm.Version as V

import qualified CommandLine.World as World
import CommandLine.World (World)


-- PATHS


stuff :: FilePath -> FilePath
stuff root =
  root </> "elm-stuff" </> compilerVersion


details :: FilePath -> FilePath
details root =
  stuff root </> "d.dat"


interfaces :: FilePath -> FilePath
interfaces root =
  stuff root </> "i.dat"


objects :: FilePath -> FilePath
objects root =
  stuff root </> "o.dat"


prepublishDir :: FilePath -> FilePath
prepublishDir root =
  stuff root </> "prepublish"


compilerVersion :: FilePath
compilerVersion =
  V.toChars V.compiler



-- ELMI and ELMO


elmi :: FilePath -> ModuleName.Raw -> FilePath
elmi root name =
  toArtifactPath root name "elmi"


elmo :: FilePath -> ModuleName.Raw -> FilePath
elmo root name =
  toArtifactPath root name "elmo"


toArtifactPath :: FilePath -> ModuleName.Raw -> String -> FilePath
toArtifactPath root name ext =
  stuff root </> ModuleName.toHyphenPath name <.> ext



-- TEMP


temp :: FilePath -> String -> FilePath
temp root ext =
  stuff root </> "temp" <.> ext



-- ROOT


findRoot :: IO (Maybe FilePath)
findRoot =
  do  dir <- Dir.getCurrentDirectory
      findRootHelp (FP.splitDirectories dir)


findRootHelp :: [String] -> IO (Maybe FilePath)
findRootHelp dirs =
  case dirs of
    [] ->
      return Nothing

    _:_ ->
      do  exists <- Dir.doesFileExist (FP.joinPath dirs </> "elm.json")
          if exists
            then return (Just (FP.joinPath dirs))
            else findRootHelp (init dirs)



-- LOCKS


withRootLock :: World m => FilePath -> m a -> m a
withRootLock root work =
  do  let dir = stuff root
      World.createDirectoryIfMissing dir
      World.withExclusiveFileLock (dir </> "lock") (\_ -> work)


withRegistryLock :: World m => PackageCache -> m a -> m a
withRegistryLock (PackageCache dir) work =
  World.withExclusiveFileLock (dir </> "lock") (\_ -> work)



-- PACKAGE CACHES


newtype PackageCache = PackageCache FilePath


getPackageCache :: World m => m PackageCache
getPackageCache =
  PackageCache <$> getCacheDir "packages"


registry :: PackageCache -> FilePath
registry (PackageCache dir) =
  dir </> "registry.dat"


package :: PackageCache -> Pkg.Name -> V.Version -> FilePath
package (PackageCache dir) name version =
  dir </> Pkg.toFilePath name </> V.toChars version



-- CACHE


getReplCache :: World m => m FilePath
getReplCache =
  getCacheDir "repl"


getCacheDir :: World m => FilePath -> m FilePath
getCacheDir projectName =
  do  home <- getElmHome
      let root = home </> compilerVersion </> projectName
      World.createDirectoryIfMissing root
      return root


getElmHome :: World m => m FilePath
getElmHome =
  do  maybeCustomHome <- World.lookupEnv "ELM_HOME"
      case maybeCustomHome of
        Just customHome -> return customHome
        Nothing -> World.getAppUserDataDirectory "elm"
