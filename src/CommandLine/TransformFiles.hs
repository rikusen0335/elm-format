module CommandLine.TransformFiles
    ( Result
    , TransformMode(..), applyTransformation
    , ValidateMode(..), validateNoChanges
    ) where

-- This module provides reusable functions for command line tools that
-- transform files.

import Control.Monad.Free
import Data.Text (Text)
import ElmFormat.FileStore (FileStore)
import ElmFormat.FileWriter (FileWriter)
import ElmFormat.InputConsole (InputConsole)
import ElmFormat.OutputConsole (OutputConsole)

import qualified ElmFormat.InputConsole as InputConsole
import qualified ElmFormat.FileStore as FileStore
import qualified ElmFormat.FileWriter as FileWriter
import qualified ElmFormat.OutputConsole as OutputConsole


data Result a
    = NoChange FilePath a
    | Changed FilePath a


checkChange :: Eq a => (FilePath, a) -> a -> Result a
checkChange (inputFile, inputText) outputText =
    if inputText == outputText
        then NoChange inputFile outputText
        else Changed inputFile outputText


updateFile :: FileWriter f => Result Text -> Free f ()
updateFile result =
    case result of
        NoChange _ _ -> return ()
        Changed outputFile outputText -> FileWriter.overwriteFile outputFile outputText


readStdin :: InputConsole f => Free f (FilePath, Text)
readStdin =
    (,) "<STDIN>" <$> InputConsole.readStdin


readFromFile :: FileStore f => (FilePath -> Free f ()) -> FilePath -> Free f (FilePath, Text)
readFromFile onProcessingFile filePath =
    onProcessingFile filePath
        *> FileStore.readFileWithPath filePath


data TransformMode
    = StdinToStdout
    | StdinToFile FilePath
    | FileToStdout FilePath
    | FileToFile FilePath FilePath
    | FilesInPlace FilePath [FilePath]


applyTransformation ::
    (InputConsole f, OutputConsole f, FileStore f, FileWriter f) =>
    (info -> Free f ())
    -> (FilePath -> info)
    -> ([FilePath] -> Free f Bool)
    -> ((FilePath, Text) -> Either info Text)
    -> TransformMode
    -> Free f Bool
applyTransformation onInfo processingFile approve transform mode =
    case mode of
        StdinToStdout ->
            (transform <$> readStdin) >>= logErrorOr onInfo OutputConsole.writeStdout

        StdinToFile outputFile ->
            (transform <$> readStdin) >>= logErrorOr onInfo (FileWriter.overwriteFile outputFile)

        -- TODO: this prints "Processing such-and-such-a-file.elm" which makes the stdout invalid
        -- FileToStdout inputFile ->
        --     (fmap fromResult <$> transform <$> ElmFormat.readFile inputFile) >>= logErrorOr OutputConsole.writeStdout

        FileToFile inputFile outputFile ->
            (transform <$> readFromFile (onInfo . processingFile) inputFile) >>= logErrorOr onInfo (FileWriter.overwriteFile outputFile)

        FilesInPlace first rest ->
            do
                canOverwrite <- approve (first:rest)
                if canOverwrite
                    then all id <$> mapM formatFile (first:rest)
                    else return True
            where
                formatFile file = ((\i -> checkChange i <$> transform i) <$> readFromFile (onInfo . processingFile) file) >>= logErrorOr onInfo updateFile


data ValidateMode
    = ValidateStdin
    | ValidateFiles FilePath [FilePath]


validateNoChanges ::
    (InputConsole f, OutputConsole f, FileStore f, FileWriter f) =>
    (info -> Free f ())
    -> (FilePath -> info)
    -> ((FilePath, Text) -> Either info ())
    -> ValidateMode
    -> Free f Bool
validateNoChanges onInfo processingFile validate mode =
    case mode of
        ValidateStdin ->
            (validate <$> readStdin) >>= logError onInfo

        ValidateFiles first rest ->
            and <$> mapM validateFile (first:rest)
            where
                validateFile file =
                    (validate <$> readFromFile (onInfo . processingFile) file)
                        >>= logError onInfo


logErrorOr :: Monad m => (error -> m ()) -> (a -> m ()) -> Either error a -> m Bool
logErrorOr onInfo fn result =
    case result of
        Left message ->
            onInfo message *> return False

        Right value ->
            fn value *> return True

logError :: Monad m => (error -> m ()) -> Either error () -> m Bool
logError onInfo =
    logErrorOr onInfo return
