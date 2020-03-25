module ElmFormat.FileStore (FileStore, FileStoreF(..), FileType(..), readFile, readFileWithPath, stat, listDirectory, execute) where

import Prelude hiding (readFile, writeFile)
import Control.Monad.Free
import Data.Text (Text)
import ElmFormat.World (World, FileType)
import qualified ElmFormat.World as World


class Functor f => FileStore f where
    readFile :: FilePath -> f Text
    stat :: FilePath -> f FileType
    listDirectory :: FilePath -> f [FilePath]

readFileWithPath :: FileStore f => FilePath -> f (FilePath, Text)
readFileWithPath filePath =
    (,) filePath <$> readFile filePath


data FileStoreF a
    = ReadFile FilePath (Text -> a)
    | Stat FilePath (FileType -> a)
    | ListDirectory FilePath ([FilePath] -> a)
    deriving (Functor)


instance FileStore FileStoreF where
    readFile path = ReadFile path id
    stat path = Stat path id
    listDirectory path = ListDirectory path id


instance FileStore f => FileStore (Free f) where
    readFile path = liftF (readFile path)
    stat path = liftF (stat path)
    listDirectory path = liftF (listDirectory path)


execute :: World m => FileStoreF a -> m a
execute operation =
    case operation of
        ReadFile path next ->
            next <$> World.readUtf8File path

        Stat path next ->
            next <$> World.stat path

        ListDirectory path next ->
            next <$> World.listDirectory path
