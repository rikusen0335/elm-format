{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TypeFamilies #-}
module CommandLine.World.IO where

import Prelude ()
import Relude hiding (getLine, putStr)

import CommandLine.World
import CommandLine.World.HttpWrapper
import qualified Control.Concurrent
import qualified Data.Text as Text
import qualified Data.Text.IO
import qualified Data.ByteString.Lazy as Lazy
import qualified ElmBuilder.BackgroundWriter
import qualified ElmBuilder.File
import qualified ElmBuilder.Http
import qualified System.Directory
import qualified System.Environment
import qualified System.Exit
import qualified System.FileLock
import qualified System.IO


instance World IO where
    type FileLock IO = System.FileLock.FileLock
    type BackgroundWriterScope IO = ElmBuilder.BackgroundWriter.Scope
    type HttpManager IO = ElmBuilder.Http.Manager

    readUtf8File path = decodeUtf8 <$> readFileBS path
    writeUtf8File path content = writeFileBS path $ encodeUtf8 content

    doesFileExist = System.Directory.doesFileExist
    doesDirectoryExist = System.Directory.doesDirectoryExist
    listDirectory = System.Directory.listDirectory
    getModificationTime = System.Directory.getModificationTime

    getProgName = fmap Text.pack System.Environment.getProgName

    getStdin = decodeUtf8 <$> toStrict <$> Lazy.getContents
    getLine = Data.Text.IO.getLine
    putStr = Data.Text.IO.putStr
    putStrLn = Data.Text.IO.putStrLn
    writeStdout content = putBS $ encodeUtf8 content
    flushStdout = System.IO.hFlush stdout
    putStrStderr = Data.Text.IO.hPutStr stderr
    putStrLnStderr = Data.Text.IO.hPutStrLn stderr

    exitFailure = System.Exit.exitFailure
    exitSuccess = System.Exit.exitSuccess

    -- Needed to support ElmCompiler/ElmBuilder
    readBinary = ElmBuilder.File.readBinary
    readFileWithUtf8 = ElmBuilder.File.readUtf8
    writeBinary = ElmBuilder.File.writeBinary
    writeBuilder = ElmBuilder.File.writeBuilder
    writeFileWithUtf8 = ElmBuilder.File.writeUtf8
    writeArchive = ElmBuilder.File.writePackage
    removeFile = ElmBuilder.File.remove
    withBackgroundWriter = ElmBuilder.BackgroundWriter.withScope
    writeBinaryBackground = ElmBuilder.BackgroundWriter.writeBinary
    getHttpWrapper =
        do
            mvar <- Control.Concurrent.newEmptyMVar
            _ <- Control.Concurrent.forkIO $ Control.Concurrent.putMVar mvar =<< ElmBuilder.Http.getManager
            return $ HttpWrapper $
                \action ->
                    Control.Concurrent.takeMVar mvar >>= action
    httpGet (HttpWrapper fetch) url headers onError onSuccess =
        fetch (\manager -> ElmBuilder.Http.get manager url headers onError onSuccess)
    httpPost (HttpWrapper fetch) url headers onError onSuccess =
        fetch (\manager -> ElmBuilder.Http.post manager url headers onError onSuccess)
    httpGetArchive (HttpWrapper fetch) url onError err onSuccess =
        fetch (\manager -> ElmBuilder.Http.getArchive manager url onError err onSuccess)
    createDirectoryIfMissing = System.Directory.createDirectoryIfMissing True
    canonicalizePath = System.Directory.canonicalizePath
    getAppUserDataDirectory = System.Directory.getAppUserDataDirectory
    lookupEnv = System.Environment.lookupEnv
    withExclusiveFileLock lockPath = System.FileLock.withFileLock lockPath System.FileLock.Exclusive
    newEmptyMVar = Control.Concurrent.newEmptyMVar
    newMVar = Control.Concurrent.newMVar
    putMVar = Control.Concurrent.putMVar
    readMVar = Control.Concurrent.readMVar
    takeMVar = Control.Concurrent.takeMVar
    fork work =
        do
            mvar <- Control.Concurrent.newEmptyMVar
            _ <- Control.Concurrent.forkIO $ Control.Concurrent.putMVar mvar =<< work
            return mvar

