{-# LANGUAGE Rank2Types #-}
module ElmFormat.Execute (forHuman, forMachine, run) where

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
import qualified ElmFormat.InputConsole as InputConsole
import qualified ElmFormat.OutputConsole as OutputConsole
import qualified Messages.Formatter.Format as InfoFormatter


data Program m opF state = Program
    { init :: (m (), state)
    , step :: forall a. opF a -> StateT state m a
    , done :: state -> m ()
    }


run :: World m => Program m opF state -> Free opF a -> m a
run program operations =
    do
        let Program init step done = program
        let (initIO, initState) = init
        initIO
        (result, finalState) <-
            operations
                |> foldFree step
                |> flip runStateT initState
        done finalState
        return result


{-| Execute Operations in a fashion appropriate for interacting with humans. -}
forHuman :: World m => Bool -> Program m OperationF Bool
forHuman autoYes =
    let
        infoMode = InfoFormatter.ForHuman
        elmVersion = undefined
    in
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


{-| Execute Operations in a fashion appropriate for use by automated scripts. -}
forMachine :: World m => ElmVersion -> Bool -> Program m OperationF Bool
forMachine elmVersion autoYes =
    let
        infoMode = InfoFormatter.ForMachine
    in
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
