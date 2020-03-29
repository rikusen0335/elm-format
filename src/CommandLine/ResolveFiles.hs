module CommandLine.ResolveFiles (resolveElmFiles, Error(..)) where

-- This module provides reusable functions to resolve command line arguments into a list of Elm files

import Prelude ()
import Relude

import CommandLine.World (World)
import Control.Monad.Free
import Data.Either.Extra (collectErrors)
import qualified Data.Text as Text
import ElmFormat.InfoFormatter (ToConsole(..))
import qualified ElmFormat.FileStore as FileStore
import qualified ElmFormat.Filesystem as FS
import qualified ElmFormat.Operation as Operation


data Error
    = FileDoesNotExist FilePath
    | NoElmFiles FilePath


instance ToConsole Error where
    toConsole = \case
        FileDoesNotExist path -> Text.pack path <> ": No such file or directory"
        NoElmFiles path -> Text.pack path <> ": Directory does not contain any *.elm files"


resolveFile :: World m => FilePath -> m (Either Error [FilePath])
resolveFile path =
    foldFree Operation.execute $
    do
        fileType <- FileStore.stat path

        case fileType of
            FileStore.IsFile ->
                return $ Right [path]

            FileStore.IsDirectory ->
                do
                    elmFiles <- FS.findAllElmFiles path
                    case elmFiles of
                        [] -> return $ Left $ NoElmFiles path
                        _ -> return $ Right elmFiles

            FileStore.DoesNotExist ->
                return $ Left $ FileDoesNotExist path


resolveElmFiles :: World m => [FilePath] -> m (Either [Error] [FilePath])
resolveElmFiles inputFiles =
    do
        result <- collectErrors <$> mapM resolveFile inputFiles
        case result of
            Left ls ->
                return $ Left ls

            Right files ->
                return $ Right $ concat files
