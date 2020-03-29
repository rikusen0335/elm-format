{-# LANGUAGE LambdaCase #-}
module ElmFormat.Operation (Operation, OperationF(..), ElmFormat.Operation.execute) where

import Prelude hiding (readFile, writeFile)
import Control.Monad.Free
import ElmFormat.FileStore as FileStore
import ElmFormat.FileWriter as FileWriter
import ElmFormat.InfoFormatter as InfoFormatter
import ElmFormat.InputConsole as InputConsole
import ElmFormat.OutputConsole as OutputConsole
import ElmFormat.World (World)


class (FileStore f, InfoFormatter f, OutputConsole f) => Operation f


data OperationF a
    = InFileStore (FileStoreF a)
    | InInfoFormatter (InfoFormatterF a)
    | InOutputConsole (OutputConsoleF a)
    | InInputConsole (InputConsoleF a)
    | InFileWriter (FileWriterF a)
    deriving (Functor)


instance Operation OperationF


instance FileStore OperationF where
    readFile path = InFileStore $ readFile path
    stat path = InFileStore $ stat path
    listDirectory path = InFileStore $ listDirectory path


instance InfoFormatter OperationF where
    putInfoToStderr text = InInfoFormatter $ putInfoToStderr text
    putInfoToStdout text = InInfoFormatter $ putInfoToStdout text
    putInfoToStdoutN text = InInfoFormatter $ putInfoToStdoutN text
    yesOrNo = InInfoFormatter yesOrNo
    empty bool = InInfoFormatter $ empty bool


instance InputConsole OperationF where
    readStdin = InInputConsole readStdin


instance OutputConsole OperationF where
    writeStdout content = InOutputConsole $ writeStdout content


instance FileWriter OperationF where
    writeFile path content = InFileWriter $ writeFile path content
    overwriteFile path content = InFileWriter $ overwriteFile path content


instance Operation f => Operation (Free f)


execute :: World m => OperationF a -> m a
execute = \case
    InFileStore op -> FileStore.execute op
    InInfoFormatter op -> InfoFormatter.execute op
    InInputConsole op -> InputConsole.execute op
    InOutputConsole op -> OutputConsole.execute op
    InFileWriter op -> FileWriter.execute op
