{-# LANGUAGE LambdaCase #-}
module ElmFormat.Execute (execute) where

{-| This module provides executors that can take streams of Operations and
perform IO.
-}

import Prelude hiding (init)
import Elm.Utils ((|>))
import Control.Monad.State
import Control.Monad.Free
import ElmFormat.Operation
import ElmFormat.World

import qualified ElmFormat.FileStore as FileStore
import qualified ElmFormat.FileWriter as FileWriter
import qualified ElmFormat.InfoFormatter as InfoFormatter
import qualified ElmFormat.InputConsole as InputConsole
import qualified ElmFormat.OutputConsole as OutputConsole


execute :: World m => InfoFormatter.ExecuteMode -> Bool -> Free OperationF a -> m a
execute infoMode autoYes operations =
    let
        init = InfoFormatter.init infoMode
        step = \case
            InFileStore op -> lift $ FileStore.execute op
            InInfoFormatter op -> InfoFormatter.step infoMode autoYes op
            InInputConsole op -> lift $ InputConsole.execute op
            InOutputConsole op -> lift $ OutputConsole.execute op
            InFileWriter op -> lift $ FileWriter.execute op
        done = InfoFormatter.done infoMode
    in
    do
        let (initIO, initState) = init
        initIO
        (result, finalState) <-
            operations
                |> foldFree step
                |> flip runStateT initState
        done finalState
        return result
