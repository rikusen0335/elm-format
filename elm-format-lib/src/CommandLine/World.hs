{-# LANGUAGE TypeFamilies #-}
module CommandLine.World where

import Prelude ()
import Relude hiding (getLine, putStr)

import qualified Codec.Archive.Zip
import CommandLine.World.HttpWrapper (HttpWrapper)
import Data.Binary (Binary)
import qualified Data.ByteString
import qualified Data.ByteString.Builder
import qualified Data.ByteString.Char8
import qualified Data.Digest.Pure.SHA
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import qualified ElmBuilder.HttpError
import qualified Network.HTTP.Types.Header


data FileType
    = IsFile
    | IsDirectory
    | DoesNotExist


class Monad m => World m where
    type FileLock m :: *
    type BackgroundWriterScope m :: *
    type HttpManager m :: *

    readUtf8File :: FilePath -> m Text
    readUtf8FileWithPath :: FilePath -> m (FilePath, Text)
    readUtf8FileWithPath filePath =
        (,) filePath <$> readUtf8File filePath
    writeUtf8File :: FilePath -> Text -> m ()
    writeUtf8FileNoOverwrite :: FilePath -> Text -> m ()
    writeUtf8FileNoOverwrite path content =
        do
            exists <- doesFileExist path
            case exists of
                True ->
                    error "file exists and was not marked to be overwritten"
                False ->
                    writeUtf8File path content

    doesFileExist :: FilePath -> m Bool
    doesDirectoryExist :: FilePath -> m Bool
    listDirectory :: FilePath -> m [FilePath]
    getModificationTime :: FilePath -> m UTCTime
    stat :: FilePath -> m FileType
    stat path =
        do
            isFile <- doesFileExist path
            isDirectory <- doesDirectoryExist path
            return $ case ( isFile, isDirectory ) of
                ( True, _ ) -> IsFile
                ( _, True ) -> IsDirectory
                ( False, False ) -> DoesNotExist

    getProgName :: m Text

    getStdin :: m Text
    getLine :: m Text
    getYesOrNo :: m Bool
    getYesOrNo =
      do  flushStdout
          input <- getLine
          case input of
            "y" -> return True
            "n" -> return False
            _   -> putStr "Must type 'y' for yes or 'n' for no: " *> getYesOrNo
    putStr :: Text -> m ()
    putStrLn :: Text -> m ()
    writeStdout :: Text -> m ()
    flushStdout :: m ()
    putStrStderr :: Text -> m ()
    putStrLnStderr :: Text -> m()

    exitFailure :: m ()
    exitSuccess :: m ()

    -- Needed to support ElmCompiler/ElmBuilder, and likely will be refactored
    readBinary :: Binary a => FilePath -> m (Maybe a)
    readFileWithUtf8 :: FilePath -> m Data.ByteString.ByteString
    writeBinary :: Binary a => FilePath -> a -> m ()
    writeBuilder :: FilePath -> Data.ByteString.Builder.Builder -> m ()
    writeFileWithUtf8 :: FilePath -> Data.ByteString.ByteString -> m ()
    writeArchive :: FilePath -> Codec.Archive.Zip.Archive -> m ()
    removeFile :: FilePath -> m ()
    withBackgroundWriter :: (BackgroundWriterScope m -> m a) -> m a
    writeBinaryBackground :: Binary a => BackgroundWriterScope m -> FilePath -> a -> m ()
    getHttpWrapper :: m (HttpWrapper m (HttpManager m))
    httpGet ::
        HttpWrapper m (HttpManager m)
        -> String
        -> [Network.HTTP.Types.Header.Header]
        -> (ElmBuilder.HttpError.Error -> e)
        -> (Data.ByteString.Char8.ByteString -> m (Either e a))
        -> m (Either e a)
    httpPost ::
        HttpWrapper m (HttpManager m)
        -> String
        -> [Network.HTTP.Types.Header.Header]
        -> (ElmBuilder.HttpError.Error -> e)
        -> (Data.ByteString.Char8.ByteString -> m (Either e a))
        -> m (Either e a)
    httpGetArchive ::
        HttpWrapper m (HttpManager m)
        -> String
        -> (ElmBuilder.HttpError.Error -> e)
        -> e
        -> ((Data.Digest.Pure.SHA.Digest Data.Digest.Pure.SHA.SHA1State, Codec.Archive.Zip.Archive) -> m (Either e a))
        -> m (Either e a)
    createDirectoryIfMissing :: FilePath -> m ()
    canonicalizePath :: FilePath -> m FilePath
    getAppUserDataDirectory :: FilePath -> m FilePath
    lookupEnv :: String -> m (Maybe String)
    withExclusiveFileLock :: FilePath -> (FileLock m -> m a) -> m a
    newEmptyMVar :: m (MVar a)
    newMVar :: a -> m (MVar a)
    putMVar :: MVar a -> a -> m ()
    readMVar :: MVar a -> m a
    takeMVar :: MVar a -> m a
    fork :: m a -> m (MVar a)
