{-# LANGUAGE LambdaCase #-}
module ElmFormat.InfoFormatter
    ( InfoFormatter, InfoFormatterF(..), onInfo, approve
    , ExecuteMode(..), init, done, step
    ) where

import Prelude hiding (init, putStrLn)

import Control.Monad.Free
import Control.Monad.State
import ElmVersion (ElmVersion)
import qualified ElmFormat.Version
import ElmFormat.World (World)
import qualified ElmFormat.World as World
import Messages.Strings (showPromptMessage, showInfoMessage)
import Messages.Types
import qualified Text.JSON as Json


class Functor f => InfoFormatter f where
    onInfo :: InfoMessage -> f ()
    approve :: PromptMessage -> f Bool


data InfoFormatterF a
    = OnInfo InfoMessage a
    | Approve PromptMessage (Bool -> a)
    deriving (Functor)


instance InfoFormatter InfoFormatterF where
    onInfo info = OnInfo info ()
    approve prompt = Approve prompt id


instance InfoFormatter f => InfoFormatter (Free f) where
    onInfo info = liftF (onInfo info)
    approve prompt = liftF (approve prompt)


data ExecuteMode
    = ForMachine ElmVersion
    | ForHuman { _usingStdout :: Bool }


init :: World m => ExecuteMode -> (m (), Bool)
init (ForMachine _) = (World.putStr "[", False)
init (ForHuman _) = (return (), undefined)


done :: World m => ExecuteMode -> Bool -> m ()
done (ForMachine _) _ = World.putStrLn "]"
done (ForHuman _) _ = return ()


step :: World m => ExecuteMode -> Bool -> InfoFormatterF a -> StateT Bool m a
step mode@(ForMachine _) autoYes infoFormatter =
    case infoFormatter of
        OnInfo info next ->
            logInfo mode info *> return next

        Approve _prompt next ->
            case autoYes of
                True -> return (next True)
                False -> return (next False)
step mode@(ForHuman usingStdout) autoYes infoFormatter =
    case infoFormatter of
        OnInfo info next ->
            logInfo mode info *> return next

        Approve prompt next ->
            lift $
            case autoYes of
                True -> return (next True)
                False ->
                    putStrLn usingStdout (showPromptMessage prompt)
                        *> fmap next yesOrNo


putStrLn :: World m => Bool -> String -> m ()
putStrLn usingStdout =
    -- we log to stdout unless it is being used for file output (in that case, we log to stderr)
    case usingStdout of
        True -> World.putStrLnStderr
        False -> World.putStrLn


logInfo :: World m => ExecuteMode -> InfoMessage -> StateT Bool m ()
logInfo mode info =
    case mode of
        ForMachine elmVersion ->
            maybe (return ()) json $ jsonMessage elmVersion info

        ForHuman usingStdout ->
            lift $ putStrLn usingStdout (showInfoMessage info)


jsonMessage :: ElmVersion -> InfoMessage -> Maybe Json.JSValue
jsonMessage elmVersion =
    let
        fileMessage filename message =
            Json.makeObj
                [ ( "path", Json.JSString $ Json.toJSString filename )
                , ( "message", Json.JSString $ Json.toJSString message )
                ]
    in
    \case
    ProcessingFile _ -> Nothing
    FileWouldChange file ->
        Just $ fileMessage file $
            "File is not formatted with elm-format-" ++ ElmFormat.Version.asString
            ++ " --elm-version=" ++ show elmVersion
    ParseError inputFile _ _ ->
        Just $ fileMessage inputFile "Error parsing the file"


json :: World m => Json.JSValue -> StateT Bool m ()
json jsvalue =
    do
        printComma <- get
        when printComma (lift $ World.putStr ",")
        lift $ World.putStrLn $ Json.encode jsvalue
        put True


yesOrNo :: World m => m Bool
yesOrNo =
  do  World.flushStdout
      input <- World.getLine
      case input of
        "y" -> return True
        "n" -> return False
        _   -> do World.putStr "Must type 'y' for yes or 'n' for no: "
                  yesOrNo
