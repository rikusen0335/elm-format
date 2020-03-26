module ElmFormat.World where

import Prelude ()
import Relude

import Data.Text (Text)
import System.IO (hFlush, hPutStr, hPutStrLn)
import qualified Data.ByteString.Lazy as Lazy
import qualified System.Directory as Dir
import qualified System.Environment
import qualified System.Exit
import qualified System.IO


data FileType
    = IsFile
    | IsDirectory
    | DoesNotExist


class Monad m => World m where
    readUtf8File :: FilePath -> m Text
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
    stat :: FilePath -> m FileType
    stat path =
        do
            isFile <- doesFileExist path
            isDirectory <- doesDirectoryExist path
            return $ case ( isFile, isDirectory ) of
                ( True, _ ) -> IsFile
                ( _, True ) -> IsDirectory
                ( False, False ) -> DoesNotExist

    getProgName :: m String

    getStdin :: m Text
    getLine :: m String
    putStr :: String -> m ()
    putStrLn :: String -> m ()
    writeStdout :: Text -> m ()
    flushStdout :: m ()
    putStrStderr :: String -> m ()
    putStrLnStderr :: String -> m()

    exitFailure :: m ()
    exitSuccess :: m ()


instance World IO where
    readUtf8File path = decodeUtf8 <$> readFileBS path
    writeUtf8File path content = writeFileBS path $ encodeUtf8 content

    doesFileExist = Dir.doesFileExist
    doesDirectoryExist = Dir.doesDirectoryExist
    listDirectory = Dir.listDirectory

    getProgName = System.Environment.getProgName

    getStdin = decodeUtf8 <$> toStrict <$> Lazy.getContents
    getLine = System.IO.getLine
    putStr = System.IO.putStr
    putStrLn = System.IO.putStrLn
    writeStdout content = putBS $ encodeUtf8 content
    flushStdout = hFlush stdout
    putStrStderr = hPutStr stderr
    putStrLnStderr = hPutStrLn stderr

    exitFailure = System.Exit.exitFailure
    exitSuccess = System.Exit.exitSuccess
