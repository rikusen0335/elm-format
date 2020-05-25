{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns, OverloadedStrings #-}
module ElmBuilder.Deps.Registry
  ( Registry(..)
  , KnownVersions(..)
  , read
  , fetch
  , update
  , latest
  , getVersions
  , getVersions'
  )
  where


import Prelude hiding (read)
import Control.Monad (liftM2)
import Data.Binary (Binary, get, put)
import qualified Data.List as List
import qualified Data.Map.Strict as Map

import qualified ElmBuilder.Deps.Website as Website
import qualified ElmCompiler.Elm.Package as Pkg
import qualified ElmCompiler.Elm.Version as V
import qualified ElmCompiler.Json.Decode as D
import qualified ElmCompiler.Parse.Primitives as P
import qualified ElmBuilder.Reporting.Exit as Exit
import qualified ElmBuilder.Stuff as Stuff

import qualified CommandLine.World as World
import CommandLine.World (World)
import CommandLine.World.IO ()
import CommandLine.World.HttpWrapper (HttpWrapper)


-- REGISTRY


data Registry =
  Registry
    { _count :: !Int
    , _versions :: !(Map.Map Pkg.Name KnownVersions)
    }


data KnownVersions =
  KnownVersions
    { _newest :: V.Version
    , _previous :: ![V.Version]
    }



-- READ


read :: World m => Stuff.PackageCache -> m (Maybe Registry)
read cache =
  World.readBinary (Stuff.registry cache)



-- FETCH


fetch :: World m => HttpWrapper m (World.HttpManager m) -> Stuff.PackageCache -> m (Either Exit.RegistryProblem Registry)
fetch manager cache =
  post manager "/all-packages" allPkgsDecoder $
    \versions ->
      do  let size = Map.foldr' addEntry 0 versions
          let registry = Registry size versions
          let path = Stuff.registry cache
          World.writeBinary path registry
          return registry


addEntry :: KnownVersions -> Int -> Int
addEntry (KnownVersions _ vs) count =
  count + 1 + length vs


allPkgsDecoder :: D.Decoder () (Map.Map Pkg.Name KnownVersions)
allPkgsDecoder =
  let
    keyDecoder =
      Pkg.keyDecoder bail

    versionsDecoder =
      D.list (D.mapError (\_ -> ()) V.decoder)

    toKnownVersions versions =
      case List.sortBy (flip compare) versions of
        v:vs -> return (KnownVersions v vs)
        []   -> D.failure ()
  in
  D.dict keyDecoder (toKnownVersions =<< versionsDecoder)



-- UPDATE


update :: World m => HttpWrapper m (World.HttpManager m) -> Stuff.PackageCache -> Registry -> m (Either Exit.RegistryProblem Registry)
update wrapper cache oldRegistry@(Registry size packages) =
  post wrapper ("/all-packages/since/" ++ show size) (D.list newPkgDecoder) $
    \news ->
      case news of
        [] ->
          return oldRegistry

        _:_ ->
          let
            newSize = size + length news
            newPkgs = foldr addNew packages news
            newRegistry = Registry newSize newPkgs
          in
          do  World.writeBinary (Stuff.registry cache) newRegistry
              return newRegistry


addNew :: (Pkg.Name, V.Version) -> Map.Map Pkg.Name KnownVersions -> Map.Map Pkg.Name KnownVersions
addNew (name, version) versions =
  let
    add maybeKnowns =
      case maybeKnowns of
        Just (KnownVersions v vs) ->
          KnownVersions version (v:vs)

        Nothing ->
          KnownVersions version []
  in
  Map.alter (Just . add) name versions



-- NEW PACKAGE DECODER


newPkgDecoder :: D.Decoder () (Pkg.Name, V.Version)
newPkgDecoder =
  D.customString newPkgParser bail


newPkgParser :: P.Parser () (Pkg.Name, V.Version)
newPkgParser =
  do  pkg <- P.specialize (\_ _ _ -> ()) Pkg.parser
      P.word1 0x40 {-@-} bail
      vsn <- P.specialize (\_ _ _ -> ()) V.parser
      return (pkg, vsn)


bail :: row -> col -> ()
bail _ _ =
  ()



-- LATEST


latest :: World m => HttpWrapper m (World.HttpManager m) -> Stuff.PackageCache -> m (Either Exit.RegistryProblem Registry)
latest wrapper cache =
  do  maybeOldRegistry <- read cache
      case maybeOldRegistry of
        Just oldRegistry ->
          update wrapper cache oldRegistry

        Nothing ->
          fetch wrapper cache



-- GET VERSIONS


getVersions :: Pkg.Name -> Registry -> Maybe KnownVersions
getVersions name (Registry _ versions) =
  Map.lookup name versions


getVersions' :: Pkg.Name -> Registry -> Either [Pkg.Name] KnownVersions
getVersions' name (Registry _ versions) =
  case Map.lookup name versions of
    Just kvs -> Right kvs
    Nothing -> Left $ Pkg.nearbyNames name (Map.keys versions)



-- POST


post :: World m => HttpWrapper m (World.HttpManager m) -> String -> D.Decoder x a -> (a -> m b) -> m (Either Exit.RegistryProblem b)
post wrapper path decoder callback =
  let
    url = Website.route path []
  in
  World.httpPost wrapper url [] Exit.RP_Http $
    \body ->
      case D.fromByteString decoder body of
        Right a -> Right <$> callback a
        Left _ -> return $ Left $ Exit.RP_Data url body



-- BINARY


instance Binary Registry where
  get = liftM2 Registry get get
  put (Registry a b) = put a >> put b


instance Binary KnownVersions where
  get = liftM2 KnownVersions get get
  put (KnownVersions a b) = put a >> put b
