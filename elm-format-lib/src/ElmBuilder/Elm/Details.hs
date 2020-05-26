{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns, OverloadedStrings #-}
module ElmBuilder.Elm.Details
  ( Details(..)
  , BuildID
  , ValidOutline(..)
  , Local(..)
  , Foreign(..)
  , Interfaces
  , load
  , loadObjects
  , loadInterfaces
  , verifyInstall
  )
  where


import Control.Concurrent.MVar (MVar)
import Control.Monad (liftM, liftM2, liftM3)
import Data.Binary (Binary, get, put, getWord8, putWord8)
import qualified Data.Either as Either
import qualified Data.Map as Map
import qualified ElmCompiler.Data.Map.Utils as Map
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Maybe as Maybe
import qualified ElmCompiler.Data.Name as Name
import qualified ElmCompiler.Data.NonEmptyList as NE
import qualified ElmCompiler.Data.OneOrMore as OneOrMore
import qualified Data.Set as Set
import qualified ElmCompiler.Data.Utf8 as Utf8
import Data.Word (Word64)
import System.FilePath ((</>), (<.>))

import qualified ElmCompiler.AST.Canonical as Can
import qualified ElmCompiler.AST.Source as Src
import qualified ElmCompiler.AST.Optimized as Opt
import qualified ElmCompiler.Compile as Compile
import qualified ElmBuilder.Deps.Registry as Registry
import qualified ElmBuilder.Deps.Solver as Solver
import qualified ElmBuilder.Deps.Website as Website
import qualified ElmCompiler.Elm.Constraint as Con
import qualified ElmCompiler.Elm.Docs as Docs
import qualified ElmCompiler.Elm.Interface as I
import qualified ElmCompiler.Elm.Kernel as Kernel
import qualified ElmCompiler.Elm.ModuleName as ModuleName
import qualified ElmBuilder.Elm.Outline as Outline
import qualified ElmCompiler.Elm.Package as Pkg
import qualified ElmCompiler.Elm.Version as V
import qualified ElmBuilder.File as File
import qualified ElmBuilder.Http as Http
import qualified ElmCompiler.Json.Decode as D
import qualified ElmCompiler.Json.Encode as E
import qualified ElmCompiler.Parse.Module as Parse
import qualified ElmBuilder.Reporting as Reporting
import qualified ElmCompiler.Reporting.Annotation as A
import qualified ElmBuilder.Reporting.Exit as Exit
import qualified ElmBuilder.Reporting.Task as Task
import qualified ElmBuilder.Stuff as Stuff

import qualified CommandLine.World as World
import CommandLine.World (World)
import CommandLine.World.HttpWrapper (HttpWrapper)


-- DETAILS


data Details =
  Details
    { _outlineTime :: File.Time
    , _outline :: ValidOutline
    , _buildID :: BuildID
    , _locals :: Map.Map ModuleName.Raw Local
    , _foreigns :: Map.Map ModuleName.Raw Foreign
    , _extras :: Extras
    }


type BuildID = Word64


data ValidOutline
  = ValidApp (NE.List Outline.SrcDir)
  | ValidPkg Pkg.Name [ModuleName.Raw] (Map.Map Pkg.Name V.Version {- for docs in reactor -})


-- NOTE: we need two ways to detect if a file must be recompiled:
--
-- (1) _time is the modification time from the last time we compiled the file.
-- By checking EQUALITY with the current modification time, we can detect file
-- saves and `git checkout` of previous versions. Both need a recompile.
--
-- (2) _lastChange is the BuildID from the last time a new interface file was
-- generated, and _lastCompile is the BuildID from the last time the file was
-- compiled. These may be different if a file is recompiled but the interface
-- stayed the same. When the _lastCompile is LESS THAN the _lastChange of any
-- imports, we need to recompile. This can happen when a project has multiple
-- entrypoints and some modules are compiled less often than their imports.
--
data Local =
  Local
    { _path :: FilePath
    , _time :: File.Time
    , _deps :: [ModuleName.Raw]
    , _main :: Bool
    , _lastChange :: BuildID
    , _lastCompile :: BuildID
    }


data Foreign =
  Foreign Pkg.Name [Pkg.Name]


data Extras
  = ArtifactsCached
  | ArtifactsFresh Interfaces Opt.GlobalGraph


type Interfaces =
  Map.Map ModuleName.Canonical I.DependencyInterface



-- LOAD ARTIFACTS


loadObjects :: World m => FilePath -> Details -> m (MVar (Maybe Opt.GlobalGraph))
loadObjects root (Details _ _ _ _ _ extras) =
  case extras of
    ArtifactsFresh _ o -> World.newMVar (Just o)
    ArtifactsCached    -> World.fork (World.readBinary (Stuff.objects root))


loadInterfaces :: World m => FilePath -> Details -> m (MVar (Maybe Interfaces))
loadInterfaces root (Details _ _ _ _ _ extras) =
  case extras of
    ArtifactsFresh i _ -> World.newMVar (Just i)
    ArtifactsCached    -> World.fork (World.readBinary (Stuff.interfaces root))



-- VERIFY INSTALL -- used by Install


verifyInstall :: World m => World.BackgroundWriterScope m -> FilePath -> Solver.Env m -> Outline.Outline -> m (Either Exit.Details ())
verifyInstall scope root (Solver.Env cache manager connection registry) outline =
  do  time <- File.getTime (root </> "elm.json")
      let key = Reporting.ignorer
      let env = Env key scope root cache manager connection registry
      case outline of
        Outline.Pkg pkg -> Task.run (verifyPkg env time pkg >> return ())
        Outline.App app -> Task.run (verifyApp env time app >> return ())



-- LOAD -- used by Make, Repl, Reactor


load :: World m => Reporting.Style -> World.BackgroundWriterScope m -> FilePath -> m (Either Exit.Details Details)
load style scope root =
  do  newTime <- File.getTime (root </> "elm.json")
      maybeDetails <- World.readBinary (Stuff.details root)
      case maybeDetails of
        Nothing ->
          generate style scope root newTime

        Just details@(Details oldTime _ buildID _ _ _) ->
          if oldTime == newTime
          then return (Right details { _buildID = buildID + 1 })
          else generate style scope root newTime



-- GENERATE


generate :: World m => Reporting.Style -> World.BackgroundWriterScope m -> FilePath -> File.Time -> m (Either Exit.Details Details)
generate style scope root time =
  Reporting.trackDetails style $ \key ->
    do  result <- initEnv key scope root
        case result of
          Left exit ->
            return (Left exit)

          Right (env, outline) ->
            case outline of
              Outline.Pkg pkg -> Task.run (verifyPkg env time pkg)
              Outline.App app -> Task.run (verifyApp env time app)



-- ENV


data Env m =
  Env
    { _key :: Reporting.DKey m
    , _scope :: World.BackgroundWriterScope m
    , _root :: FilePath
    , _cache :: Stuff.PackageCache
    , _manager :: HttpWrapper m (World.HttpManager m)
    , _connection :: Solver.Connection m
    , _registry :: Registry.Registry
    }


initEnv :: World m => Reporting.DKey m -> World.BackgroundWriterScope m -> FilePath -> m (Either Exit.Details (Env m, Outline.Outline))
initEnv key scope root =
  do  mvar <- World.fork Solver.initEnv
      eitherOutline <- Outline.read root
      case eitherOutline of
        Left problem ->
          return $ Left $ Exit.DetailsBadOutline problem

        Right outline ->
          do  maybeEnv <- World.readMVar mvar
              case maybeEnv of
                Left problem ->
                  return $ Left $ Exit.DetailsCannotGetRegistry problem

                Right (Solver.Env cache manager connection registry) ->
                  return $ Right (Env key scope root cache manager connection registry, outline)



-- VERIFY PROJECT


type Task m a = Task.Task m Exit.Details a


verifyPkg :: World m => Env m -> File.Time -> Outline.PkgOutline -> Task m Details
verifyPkg env time (Outline.PkgOutline pkg _ _ _ exposed direct testDirect elm) =
  if Con.goodElm elm
  then
    do  solution <- verifyConstraints env =<< union noDups direct testDirect
        let exposedList = Outline.flattenExposed exposed
        let exactDeps = Map.map (\(Solver.Details v _) -> v) solution -- for pkg docs in reactor
        verifyDependencies env time (ValidPkg pkg exposedList exactDeps) solution direct
  else
    Task.throw $ Exit.DetailsBadElmInPkg elm


verifyApp :: World m => Env m -> File.Time -> Outline.AppOutline -> Task m Details
verifyApp env time outline@(Outline.AppOutline elmVersion srcDirs direct _ _ _) =
  if elmVersion == V.compiler
  then
    do  stated <- checkAppDeps outline
        actual <- verifyConstraints env (Map.map Con.exactly stated)
        if Map.size stated == Map.size actual
          then verifyDependencies env time (ValidApp srcDirs) actual direct
          else Task.throw $ Exit.DetailsHandEditedDependencies
  else
    Task.throw $ Exit.DetailsBadElmInAppOutline elmVersion


checkAppDeps :: Outline.AppOutline -> Task m (Map.Map Pkg.Name V.Version)
checkAppDeps (Outline.AppOutline _ _ direct indirect testDirect testIndirect) =
  do  x <- union allowEqualDups indirect testDirect
      y <- union noDups direct testIndirect
      union noDups x y



-- VERIFY CONSTRAINTS


verifyConstraints :: World m => Env m -> Map.Map Pkg.Name Con.Constraint -> Task m (Map.Map Pkg.Name Solver.Details)
verifyConstraints (Env _ _ _ cache _ connection registry) constraints =
  do  result <- Task.io $ Solver.verify cache connection registry constraints
      case result of
        Solver.Ok details        -> return details
        Solver.NoSolution        -> Task.throw $ Exit.DetailsNoSolution
        Solver.NoOfflineSolution -> Task.throw $ Exit.DetailsNoOfflineSolution
        Solver.Err exit          -> Task.throw $ Exit.DetailsSolverProblem exit



-- UNION


union :: (Ord k) => (k -> v -> v -> Task m v) -> Map.Map k v -> Map.Map k v -> Task m (Map.Map k v)
union tieBreaker deps1 deps2 =
  Map.mergeA Map.preserveMissing Map.preserveMissing (Map.zipWithAMatched tieBreaker) deps1 deps2


noDups :: k -> v -> v -> Task m v
noDups _ _ _ =
  Task.throw Exit.DetailsHandEditedDependencies


allowEqualDups :: (Eq v) => k -> v -> v -> Task m v
allowEqualDups _ v1 v2 =
  if v1 == v2
  then return v1
  else Task.throw Exit.DetailsHandEditedDependencies



-- VERIFY DEPENDENCIES


verifyDependencies :: World m => Env m -> File.Time -> ValidOutline -> Map.Map Pkg.Name Solver.Details -> Map.Map Pkg.Name a -> Task m Details
verifyDependencies env@(Env key scope root cache _ _ _) time outline solution directDeps =
  Task.eio id $
  do  Reporting.report key (Reporting.DStart (Map.size solution))
      mvar <- World.newEmptyMVar
      mvars <- Stuff.withRegistryLock cache $
        Map.traverseWithKey (\k v -> World.fork (verifyDep env mvar solution k v)) solution
      World.putMVar mvar mvars
      deps <- traverse World.readMVar mvars
      case sequence deps of
        Left _ ->
          do  home <- Stuff.getElmHome
              return $ Left $ Exit.DetailsBadDeps home $
                Maybe.catMaybes $ Either.lefts $ Map.elems deps

        Right artifacts ->
          let
            objs = Map.foldr addObjects Opt.empty artifacts
            ifaces = Map.foldrWithKey (addInterfaces directDeps) Map.empty artifacts
            foreigns = Map.map (OneOrMore.destruct Foreign) $ Map.foldrWithKey gatherForeigns Map.empty $ Map.intersection artifacts directDeps
            details = Details time outline 0 Map.empty foreigns (ArtifactsFresh ifaces objs)
          in
          do  World.writeBinaryBackground scope (Stuff.objects    root) objs
              World.writeBinaryBackground scope (Stuff.interfaces root) ifaces
              World.writeBinaryBackground scope (Stuff.details    root) details
              return (Right details)


addObjects :: Artifacts -> Opt.GlobalGraph -> Opt.GlobalGraph
addObjects (Artifacts _ objs) graph =
  Opt.addGlobalGraph objs graph


addInterfaces :: Map.Map Pkg.Name a -> Pkg.Name -> Artifacts -> Interfaces -> Interfaces
addInterfaces directDeps pkg (Artifacts ifaces _) dependencyInterfaces =
  Map.union dependencyInterfaces $ Map.mapKeysMonotonic (ModuleName.Canonical pkg) $
    if Map.member pkg directDeps
      then ifaces
      else Map.map I.privatize ifaces


gatherForeigns :: Pkg.Name -> Artifacts -> Map.Map ModuleName.Raw (OneOrMore.OneOrMore Pkg.Name) -> Map.Map ModuleName.Raw (OneOrMore.OneOrMore Pkg.Name)
gatherForeigns pkg (Artifacts ifaces _) foreigns =
  let
    isPublic di =
      case di of
        I.Public _      -> Just (OneOrMore.one pkg)
        I.Private _ _ _ -> Nothing
  in
  Map.unionWith OneOrMore.more foreigns (Map.mapMaybe isPublic ifaces)



-- VERIFY DEPENDENCY


data Artifacts =
  Artifacts
    { _ifaces :: Map.Map ModuleName.Raw I.DependencyInterface
    , _objects :: Opt.GlobalGraph
    }


type Dep =
  Either (Maybe Exit.DetailsBadDep) Artifacts


verifyDep :: World m => Env m -> MVar (Map.Map Pkg.Name (MVar Dep)) -> Map.Map Pkg.Name Solver.Details -> Pkg.Name -> Solver.Details -> m Dep
verifyDep (Env key _ _ cache manager _ _) depsMVar solution pkg details@(Solver.Details vsn directDeps) =
  do  let fingerprint = Map.intersectionWith (\(Solver.Details v _) _ -> v) solution directDeps
      exists <- World.doesDirectoryExist (Stuff.package cache pkg vsn </> "src")
      if exists
        then
          do  Reporting.report key Reporting.DCached
              maybeCache <- World.readBinary (Stuff.package cache pkg vsn </> "artifacts.dat")
              case maybeCache of
                Nothing ->
                  build key cache depsMVar pkg details fingerprint Set.empty

                Just (ArtifactCache fingerprints artifacts) ->
                  if Set.member fingerprint fingerprints
                    then Reporting.report key Reporting.DBuilt >> return (Right artifacts)
                    else build key cache depsMVar pkg details fingerprint fingerprints
        else
          do  Reporting.report key Reporting.DRequested
              result <- downloadPackage cache manager pkg vsn
              case result of
                Left problem ->
                  do  Reporting.report key (Reporting.DFailed pkg vsn)
                      return $ Left $ Just $ Exit.BD_BadDownload pkg vsn problem

                Right () ->
                  do  Reporting.report key (Reporting.DReceived pkg vsn)
                      build key cache depsMVar pkg details fingerprint Set.empty



-- ARTIFACT CACHE


data ArtifactCache =
  ArtifactCache
    { _fingerprints :: Set.Set Fingerprint
    , _artifacts :: Artifacts
    }


type Fingerprint =
  Map.Map Pkg.Name V.Version



-- BUILD


build :: World m => Reporting.DKey m -> Stuff.PackageCache -> MVar (Map.Map Pkg.Name (MVar Dep)) -> Pkg.Name -> Solver.Details -> Fingerprint -> Set.Set Fingerprint -> m Dep
build key cache depsMVar pkg (Solver.Details vsn _) f fs =
  do  eitherOutline <- Outline.read (Stuff.package cache pkg vsn)
      case eitherOutline of
        Left _ ->
          do  Reporting.report key Reporting.DBroken
              return $ Left $ Just $ Exit.BD_BadBuild pkg vsn f

        Right (Outline.App _) ->
          do  Reporting.report key Reporting.DBroken
              return $ Left $ Just $ Exit.BD_BadBuild pkg vsn f

        Right (Outline.Pkg (Outline.PkgOutline _ _ _ _ exposed deps _ _)) ->
          do  allDeps <- World.readMVar depsMVar
              directDeps <- traverse World.readMVar (Map.intersection allDeps deps)
              case sequence directDeps of
                Left _ ->
                  do  Reporting.report key Reporting.DBroken
                      return $ Left $ Nothing

                Right directArtifacts ->
                  do  let src = Stuff.package cache pkg vsn </> "src"
                      let foreignDeps = gatherForeignInterfaces directArtifacts
                      let exposedDict = Map.fromKeys (\_ -> ()) (Outline.flattenExposed exposed)
                      docsStatus <- getDocsStatus cache pkg vsn
                      mvar <- World.newEmptyMVar
                      mvars <- Map.traverseWithKey (const . World.fork . crawlModule foreignDeps mvar pkg src docsStatus) exposedDict
                      World.putMVar mvar mvars
                      mapM_ World.readMVar mvars
                      maybeStatuses <- traverse World.readMVar =<< World.readMVar mvar
                      case sequence maybeStatuses of
                        Nothing ->
                          do  Reporting.report key Reporting.DBroken
                              return $ Left $ Just $ Exit.BD_BadBuild pkg vsn f

                        Just statuses ->
                          do  rmvar <- World.newEmptyMVar
                              rmvars <- traverse (World.fork . compile pkg rmvar) statuses
                              World.putMVar rmvar rmvars
                              maybeResults <- traverse World.readMVar rmvars
                              case sequence maybeResults of
                                Nothing ->
                                  do  Reporting.report key Reporting.DBroken
                                      return $ Left $ Just $ Exit.BD_BadBuild pkg vsn f

                                Just results ->
                                  let
                                    path = Stuff.package cache pkg vsn </> "artifacts.dat"
                                    ifaces = gatherInterfaces exposedDict results
                                    objects = gatherObjects results
                                    artifacts = Artifacts ifaces objects
                                    fingerprints = Set.insert f fs
                                  in
                                  do  writeDocs cache pkg vsn docsStatus results
                                      World.writeBinary path (ArtifactCache fingerprints artifacts)
                                      Reporting.report key Reporting.DBuilt
                                      return (Right artifacts)



-- GATHER


gatherObjects :: Map.Map ModuleName.Raw Result -> Opt.GlobalGraph
gatherObjects results =
  Map.foldrWithKey addLocalGraph Opt.empty results


addLocalGraph :: ModuleName.Raw -> Result -> Opt.GlobalGraph -> Opt.GlobalGraph
addLocalGraph name status graph =
  case status of
    RLocal _ objs _ -> Opt.addLocalGraph objs graph
    RForeign _      -> graph
    RKernelLocal cs -> Opt.addKernel (Name.getKernel name) cs graph
    RKernelForeign  -> graph


gatherInterfaces :: Map.Map ModuleName.Raw () -> Map.Map ModuleName.Raw Result -> Map.Map ModuleName.Raw I.DependencyInterface
gatherInterfaces exposed artifacts =
  let
    onLeft  = Map.mapMissing (error "compiler bug manifesting in Elm.Details.gatherInterfaces")
    onRight = Map.mapMaybeMissing     (\_    iface -> toLocalInterface I.private iface)
    onBoth  = Map.zipWithMaybeMatched (\_ () iface -> toLocalInterface I.public  iface)
  in
  Map.merge onLeft onRight onBoth exposed artifacts


toLocalInterface :: (I.Interface -> a) -> Result -> Maybe a
toLocalInterface func result =
  case result of
    RLocal iface _ _ -> Just (func iface)
    RForeign _       -> Nothing
    RKernelLocal _   -> Nothing
    RKernelForeign   -> Nothing



-- GATHER FOREIGN INTERFACES


data ForeignInterface
  = ForeignAmbiguous
  | ForeignSpecific I.Interface


gatherForeignInterfaces :: Map.Map Pkg.Name Artifacts -> Map.Map ModuleName.Raw ForeignInterface
gatherForeignInterfaces directArtifacts =
    Map.map (OneOrMore.destruct finalize) $
      Map.foldrWithKey gather Map.empty directArtifacts
  where
    finalize :: I.Interface -> [I.Interface] -> ForeignInterface
    finalize i is =
      case is of
        [] -> ForeignSpecific i
        _:_ -> ForeignAmbiguous

    gather :: Pkg.Name -> Artifacts -> Map.Map ModuleName.Raw (OneOrMore.OneOrMore I.Interface) -> Map.Map ModuleName.Raw (OneOrMore.OneOrMore I.Interface)
    gather _ (Artifacts ifaces _) buckets =
      Map.unionWith OneOrMore.more buckets (Map.mapMaybe isPublic ifaces)

    isPublic :: I.DependencyInterface -> Maybe (OneOrMore.OneOrMore I.Interface)
    isPublic di =
      case di of
        I.Public iface  -> Just (OneOrMore.one iface)
        I.Private _ _ _ -> Nothing



-- CRAWL


type StatusDict =
  Map.Map ModuleName.Raw (MVar (Maybe Status))


data Status
  = SLocal DocsStatus (Map.Map ModuleName.Raw ()) Src.Module
  | SForeign I.Interface
  | SKernelLocal [Kernel.Chunk]
  | SKernelForeign


crawlModule :: World m => Map.Map ModuleName.Raw ForeignInterface -> MVar StatusDict -> Pkg.Name -> FilePath -> DocsStatus -> ModuleName.Raw -> m (Maybe Status)
crawlModule foreignDeps mvar pkg src docsStatus name =
  do  let path = src </> ModuleName.toFilePath name <.> "elm"
      exists <- World.doesFileExist path
      case Map.lookup name foreignDeps of
        Just ForeignAmbiguous ->
          return Nothing

        Just (ForeignSpecific iface) ->
          if exists
          then return Nothing
          else return (Just (SForeign iface))

        Nothing ->
          if exists then
            crawlFile foreignDeps mvar pkg src docsStatus name path

          else if Pkg.isKernel pkg && Name.isKernel name then
            crawlKernel foreignDeps mvar pkg src name

          else
            return Nothing


crawlFile :: World m => Map.Map ModuleName.Raw ForeignInterface -> MVar StatusDict -> Pkg.Name -> FilePath -> DocsStatus -> ModuleName.Raw -> FilePath -> m (Maybe Status)
crawlFile foreignDeps mvar pkg src docsStatus expectedName path =
  do  bytes <- World.readFileWithUtf8 path
      case Parse.fromByteString (Parse.Package pkg) bytes of
        Right modul@(Src.Module (Just (A.At _ actualName)) _ _ imports _ _ _ _ _) | expectedName == actualName ->
          do  deps <- crawlImports foreignDeps mvar pkg src imports
              return (Just (SLocal docsStatus deps modul))

        _ ->
          return Nothing


crawlImports :: World m => Map.Map ModuleName.Raw ForeignInterface -> MVar StatusDict -> Pkg.Name -> FilePath -> [Src.Import] -> m (Map.Map ModuleName.Raw ())
crawlImports foreignDeps mvar pkg src imports =
  do  statusDict <- World.takeMVar mvar
      let deps = Map.fromList (map (\i -> (Src.getImportName i, ())) imports)
      let news = Map.difference deps statusDict
      mvars <- Map.traverseWithKey (const . World.fork . crawlModule foreignDeps mvar pkg src DocsNotNeeded) news
      World.putMVar mvar (Map.union mvars statusDict)
      mapM_ World.readMVar mvars
      return deps


crawlKernel :: World m => Map.Map ModuleName.Raw ForeignInterface -> MVar StatusDict -> Pkg.Name -> FilePath -> ModuleName.Raw -> m (Maybe Status)
crawlKernel foreignDeps mvar pkg src name =
  do  let path = src </> ModuleName.toFilePath name <.> "js"
      exists <- World.doesFileExist path
      if exists
        then
          do  bytes <- World.readFileWithUtf8 path
              case Kernel.fromByteString pkg (Map.mapMaybe getDepHome foreignDeps) bytes of
                Nothing ->
                  return Nothing

                Just (Kernel.Content imports chunks) ->
                  do  _ <- crawlImports foreignDeps mvar pkg src imports
                      return (Just (SKernelLocal chunks))
        else
          return (Just SKernelForeign)


getDepHome :: ForeignInterface -> Maybe Pkg.Name
getDepHome fi =
  case fi of
    ForeignSpecific (I.Interface pkg _ _ _ _) -> Just pkg
    ForeignAmbiguous                          -> Nothing



-- COMPILE


data Result
  = RLocal !I.Interface !Opt.LocalGraph (Maybe Docs.Module)
  | RForeign I.Interface
  | RKernelLocal [Kernel.Chunk]
  | RKernelForeign


compile :: World m => Pkg.Name -> MVar (Map.Map ModuleName.Raw (MVar (Maybe Result))) -> Status -> m (Maybe Result)
compile pkg mvar status =
  case status of
    SLocal docsStatus deps modul ->
      do  resultsDict <- World.readMVar mvar
          maybeResults <- traverse World.readMVar (Map.intersection resultsDict deps)
          case sequence maybeResults of
            Nothing ->
              return Nothing

            Just results ->
              case Compile.compile pkg (Map.mapMaybe getInterface results) modul of
                Left _ ->
                  return Nothing

                Right (Compile.Artifacts canonical annotations objects) ->
                  let
                    ifaces = I.fromModule pkg canonical annotations
                    docs = makeDocs docsStatus canonical
                  in
                  return (Just (RLocal ifaces objects docs))

    SForeign iface ->
      return (Just (RForeign iface))

    SKernelLocal chunks ->
      return (Just (RKernelLocal chunks))

    SKernelForeign ->
      return (Just RKernelForeign)


getInterface :: Result -> Maybe I.Interface
getInterface result =
  case result of
    RLocal iface _ _ -> Just iface
    RForeign iface   -> Just iface
    RKernelLocal _   -> Nothing
    RKernelForeign   -> Nothing



-- MAKE DOCS


data DocsStatus
  = DocsNeeded
  | DocsNotNeeded


getDocsStatus :: World m => Stuff.PackageCache -> Pkg.Name -> V.Version -> m DocsStatus
getDocsStatus cache pkg vsn =
  do  exists <- World.doesFileExist (Stuff.package cache pkg vsn </> "docs.json")
      if exists
        then return DocsNotNeeded
        else return DocsNeeded


makeDocs :: DocsStatus -> Can.Module -> Maybe Docs.Module
makeDocs status modul =
  case status of
    DocsNeeded ->
      case Docs.fromModule modul of
        Right docs -> Just docs
        Left _     -> Nothing

    DocsNotNeeded ->
      Nothing


writeDocs :: World m => Stuff.PackageCache -> Pkg.Name -> V.Version -> DocsStatus -> Map.Map ModuleName.Raw Result -> m ()
writeDocs cache pkg vsn status results =
  case status of
    DocsNeeded ->
      E.writeUgly (Stuff.package cache pkg vsn </> "docs.json") $
        Docs.encode $ Map.mapMaybe toDocs results

    DocsNotNeeded ->
      return ()


toDocs :: Result -> Maybe Docs.Module
toDocs result =
  case result of
    RLocal _ _ docs -> docs
    RForeign _      -> Nothing
    RKernelLocal _  -> Nothing
    RKernelForeign  -> Nothing



-- DOWNLOAD PACKAGE


downloadPackage :: World m => Stuff.PackageCache -> HttpWrapper m (World.HttpManager m) -> Pkg.Name -> V.Version -> m (Either Exit.PackageProblem ())
downloadPackage cache manager pkg vsn =
  let
    url = Website.metadata pkg vsn "endpoint.json"
  in
  do  eitherByteString <-
        World.httpGet manager url [] id (return . Right)

      case eitherByteString of
        Left err ->
          return $ Left $ Exit.PP_BadEndpointRequest err

        Right byteString ->
          case D.fromByteString endpointDecoder byteString of
            Left _ ->
              return $ Left $ Exit.PP_BadEndpointContent url

            Right (endpoint, expectedHash) ->
              World.httpGetArchive manager endpoint Exit.PP_BadArchiveRequest (Exit.PP_BadArchiveContent endpoint) $
                \(sha, archive) ->
                  if expectedHash == Http.shaToChars sha
                  then Right <$> World.writeArchive (Stuff.package cache pkg vsn) archive
                  else return $ Left $ Exit.PP_BadArchiveHash endpoint expectedHash (Http.shaToChars sha)


endpointDecoder :: D.Decoder e (String, String)
endpointDecoder =
  do  url <- D.field "url" D.string
      hash <- D.field "hash" D.string
      return (Utf8.toChars url, Utf8.toChars hash)



-- BINARY


instance Binary Details where
  put (Details a b c d e _) = put a >> put b >> put c >> put d >> put e
  get =
    do  a <- get
        b <- get
        c <- get
        d <- get
        e <- get
        return (Details a b c d e ArtifactsCached)


instance Binary ValidOutline where
  put outline =
    case outline of
      ValidApp a     -> putWord8 0 >> put a
      ValidPkg a b c -> putWord8 1 >> put a >> put b >> put c

  get =
    do  n <- getWord8
        case n of
          0 -> liftM  ValidApp get
          1 -> liftM3 ValidPkg get get get
          _ -> fail "binary encoding of ValidOutline was corrupted"


instance Binary Local where
  put (Local a b c d e f) = put a >> put b >> put c >> put d >> put e >> put f
  get =
    do  a <- get
        b <- get
        c <- get
        d <- get
        e <- get
        f <- get
        return (Local a b c d e f)


instance Binary Foreign where
  get = liftM2 Foreign get get
  put (Foreign a b) = put a >> put b


instance Binary Artifacts where
  get = liftM2 Artifacts get get
  put (Artifacts a b) = put a >> put b


instance Binary ArtifactCache where
  get = liftM2 ArtifactCache get get
  put (ArtifactCache a b) = put a >> put b
