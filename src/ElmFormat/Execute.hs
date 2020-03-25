{-# LANGUAGE Rank2Types #-}
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
import ElmVersion

import qualified ElmFormat.FileStore as FileStore
import qualified ElmFormat.FileWriter as FileWriter
import qualified ElmFormat.InfoFormatter as InfoFormatter
import qualified ElmFormat.InputConsole as InputConsole
import qualified ElmFormat.OutputConsole as OutputConsole


data Program m opF state = Program
    { init :: (m (), state)
    , step :: forall a. opF a -> StateT state m a
    , done :: state -> m ()
    }


execute :: World m => InfoFormatter.ExecuteMode -> ElmVersion -> Bool -> Free OperationF a -> m a
execute infoMode elmVersion autoYes operations =
    do
        let Program init step done = program infoMode elmVersion autoYes
        let (initIO, initState) = init
        initIO
        (result, finalState) <-
            operations
                |> foldFree step
                |> flip runStateT initState
        done finalState
        return result


program :: World m => InfoFormatter.ExecuteMode -> ElmVersion -> Bool -> Program m OperationF Bool
program infoMode elmVersion autoYes =
    Program
        { init = InfoFormatter.init infoMode
        , step = \operation ->
            case operation of
                InFileStore op -> lift $ FileStore.execute op
                InInfoFormatter op -> InfoFormatter.step infoMode elmVersion autoYes op
                InInputConsole op -> lift $ InputConsole.execute op
                InOutputConsole op -> lift $ OutputConsole.execute op
                InFileWriter op -> lift $ FileWriter.execute op
        , done = InfoFormatter.done infoMode
        }
