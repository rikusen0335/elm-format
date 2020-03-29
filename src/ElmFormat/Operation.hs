module ElmFormat.Operation (Operation, OperationF(..)) where

import Prelude hiding (readFile, writeFile)
import Control.Monad.Free
import ElmFormat.FileStore
import ElmFormat.FileWriter
import ElmFormat.InfoFormatter
import ElmFormat.InputConsole
import ElmFormat.OutputConsole


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
