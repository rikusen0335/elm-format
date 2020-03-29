module CommandLine.TransformFiles
    ( Result
    , TransformMode(..), applyTransformation
    , ValidateMode(..), validateNoChanges
    ) where

-- This module provides reusable functions for command line tools that
-- transform files.

import Control.Monad.Free
import Control.Monad.State hiding (runState)
import Data.Text (Text)
import ElmFormat.FileStore (FileStore)
import ElmFormat.FileWriter (FileWriter)
import ElmFormat.InfoFormatter (ExecuteMode(..))
import ElmFormat.InputConsole (InputConsole)
import qualified ElmFormat.Operation as Operation
import ElmFormat.World (World)
import ElmVersion

import qualified ElmFormat.InfoFormatter as InfoFormatter
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


readFromFile :: FileStore f => (FilePath -> StateT s (Free f) ()) -> FilePath -> StateT s (Free f) (FilePath, Text)
readFromFile onProcessingFile filePath =
    onProcessingFile filePath
        *> lift (FileStore.readFileWithPath filePath)


data TransformMode
    = StdinToStdout
    | StdinToFile FilePath
    | FileToStdout FilePath
    | FileToFile FilePath FilePath
    | FilesInPlace FilePath [FilePath]


applyTransformation ::
    World m =>
    InfoFormatter.Loggable info =>
    (FilePath -> info)
    -> Bool
    -> ([FilePath] -> Text)
    -> ((FilePath, Text) -> Either info Text)
    -> TransformMode
    -> m Bool
applyTransformation processingFile autoYes confirmPrompt transform mode =
    let
        usesStdout =
            case mode of
                StdinToStdout -> True
                StdinToFile _ -> True
                FileToStdout _ -> True
                FileToFile _ _ -> False
                FilesInPlace _ _ -> False

        infoMode = ForHuman usesStdout

        onInfo = InfoFormatter.onInfo infoMode

        approve = InfoFormatter.approve infoMode autoYes . confirmPrompt
    in
    foldFree Operation.execute $
    runState (InfoFormatter.init infoMode) (InfoFormatter.done infoMode) $
    case mode of
        StdinToStdout ->
            lift (transform <$> readStdin) >>= logErrorOr onInfo (lift . OutputConsole.writeStdout)

        StdinToFile outputFile ->
            lift (transform <$> readStdin) >>= logErrorOr onInfo (lift . FileWriter.overwriteFile outputFile)

        FileToStdout inputFile ->
            lift (transform <$> FileStore.readFileWithPath inputFile) >>= logErrorOr onInfo (lift . OutputConsole.writeStdout)

        FileToFile inputFile outputFile ->
            (transform <$> readFromFile (onInfo . processingFile) inputFile) >>= logErrorOr onInfo (lift . FileWriter.overwriteFile outputFile)

        FilesInPlace first rest ->
            do
                canOverwrite <- lift $ approve (first:rest)
                if canOverwrite
                    then all id <$> mapM formatFile (first:rest)
                    else return True
            where
                formatFile file = ((\i -> checkChange i <$> transform i) <$> readFromFile (onInfo . processingFile) file) >>= logErrorOr onInfo (lift . updateFile)


data ValidateMode
    = ValidateStdin
    | ValidateFiles FilePath [FilePath]


validateNoChanges ::
    World m =>
    InfoFormatter.Loggable info =>
    ElmVersion
    -> (FilePath -> info)
    -> ((FilePath, Text) -> Either info ())
    -> ValidateMode
    -> m Bool
validateNoChanges elmVersion processingFile validate mode =
    let
        infoMode = ForMachine elmVersion
        onInfo = InfoFormatter.onInfo infoMode
    in
    foldFree Operation.execute $
    runState (InfoFormatter.init infoMode) (InfoFormatter.done infoMode) $
    case mode of
        ValidateStdin ->
            lift (validate <$> readStdin) >>= logError onInfo

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



runState :: Monad m => (m (), state) -> (state -> m ()) -> StateT state m result -> m result
runState (initM, initialState) done run =
    do
        initM
        (result, finalState) <- runStateT run initialState
        done finalState
        return result
