module ElmFormat.FileWriter (FileWriter, FileWriterF(..), writeFile, overwriteFile, execute) where

import Prelude hiding (writeFile)

import CommandLine.World (World)
import qualified CommandLine.World as World
import Control.Monad.Free
import Data.Text (Text)


class Functor f => FileWriter f where
    writeFile :: FilePath -> Text -> f ()
    overwriteFile :: FilePath -> Text -> f ()


data FileWriterF a
    = WriteFile FilePath Text a
    | OverwriteFile FilePath Text a
    deriving (Functor)


instance FileWriter FileWriterF where
    writeFile path content = WriteFile path content ()
    overwriteFile path content = OverwriteFile path content ()


instance FileWriter f => FileWriter (Free f) where
    writeFile path content = liftF (writeFile path content)
    overwriteFile path content = liftF (overwriteFile path content)


execute :: World m => FileWriterF a -> m a
execute operation =
    case operation of
        WriteFile path content next ->
            World.writeUtf8FileNoOverwrite path content *> return next

        OverwriteFile path content next ->
            World.writeUtf8File path content *> return next
