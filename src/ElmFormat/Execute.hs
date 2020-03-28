{-# LANGUAGE LambdaCase #-}
module ElmFormat.Execute (execute) where

{-| This module provides executors that can take streams of Operations and
perform IO.
-}

import Prelude hiding (init)
import ElmFormat.Operation
import ElmFormat.World

import qualified ElmFormat.FileStore as FileStore
import qualified ElmFormat.FileWriter as FileWriter
import qualified ElmFormat.InfoFormatter as InfoFormatter
import qualified ElmFormat.InputConsole as InputConsole
import qualified ElmFormat.OutputConsole as OutputConsole


-- TODO: move this to Operation
execute :: World m => OperationF a -> m a
execute = \case
    InFileStore op -> FileStore.execute op
    InInfoFormatter op -> InfoFormatter.execute op
    InInputConsole op -> InputConsole.execute op
    InOutputConsole op -> OutputConsole.execute op
    InFileWriter op -> FileWriter.execute op
