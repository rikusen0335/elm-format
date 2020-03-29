module CommandLine.ResolveFiles (resolveElmFiles, ResolveFileError(..)) where

-- This module provides reusable functions to resolve command line arguments into a list of Elm files

import Prelude ()
import Relude

import Control.Monad.Free
import qualified ElmFormat.FileStore as FileStore
import qualified ElmFormat.Filesystem as FS
import qualified ElmFormat.Operation as Operation
import ElmFormat.World (World)


data ResolveFileError
    = FileDoesNotExist FilePath
    | NoElmFiles FilePath


resolveFile :: World m => FilePath -> m (Either ResolveFileError [FilePath])
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


collectErrors :: [Either l r] -> Either [l] [r]
collectErrors list =
    let
        step acc next =
            case (next, acc) of
                (Left l, Right _) ->
                    Left [l]

                (Left l, Left ls) ->
                    Left (l : ls)

                (Right r, Right rs) ->
                    Right (r : rs)

                (Right _, Left ls) ->
                    Left ls
    in
        foldl' step (Right []) list


resolveElmFiles :: World m => [FilePath] -> m (Either [ResolveFileError] [FilePath])
resolveElmFiles inputFiles =
    do
        result <- collectErrors <$> mapM resolveFile inputFiles
        case result of
            Left ls ->
                return $ Left ls

            Right files ->
                return $ Right $ concat files
