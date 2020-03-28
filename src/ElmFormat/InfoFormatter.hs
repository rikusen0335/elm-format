{-# LANGUAGE LambdaCase #-}
module ElmFormat.InfoFormatter
    ( onInfo, approve
    , InfoFormatter(..), InfoFormatterF(..)
    , ExecuteMode(..), init, done, step
    ) where

import Prelude hiding (init, putStrLn)

import Control.Monad.Free
import Control.Monad.State
import Data.Text (Text)
import qualified Data.Text as Text
import ElmVersion (ElmVersion)
import ElmFormat.World (World)
import qualified ElmFormat.World as World
import Messages.Strings (showPromptMessage, showInfoMessage, jsonInfoMessage)
import Messages.Types
import qualified Text.JSON as Json


onInfo :: InfoFormatter f => InfoMessage -> f ()
onInfo = onInfo_


approve :: InfoFormatter f => ExecuteMode -> Bool -> PromptMessage -> f Bool
approve mode autoYes prompt =
    case autoYes of
        True -> empty True

        False ->
            case mode of
                ForMachine _ -> empty False

                ForHuman usingStdout ->
                    yesOrNo usingStdout (Text.pack $ showPromptMessage prompt)


class Functor f => InfoFormatter f where
    onInfo_ :: InfoMessage -> f ()
    yesOrNo :: Bool -> Text -> f Bool
    empty :: Bool -> f Bool


data InfoFormatterF a
    = OnInfo InfoMessage a
    | YesOrNo Bool Text (Bool -> a)
    | Empty a
    deriving (Functor)


instance InfoFormatter InfoFormatterF where
    onInfo_ info = OnInfo info ()
    yesOrNo usingStdout prompt = YesOrNo usingStdout prompt id
    empty bool = Empty bool


instance InfoFormatter f => InfoFormatter (Free f) where
    onInfo_ info = liftF (onInfo_ info)
    yesOrNo usingStdout prompt = liftF (yesOrNo usingStdout prompt)
    empty bool = liftF (empty bool)


data ExecuteMode
    = ForMachine ElmVersion
    | ForHuman { _usingStdout :: Bool }


init :: World m => ExecuteMode -> (m (), Bool)
init (ForMachine _) = (World.putStr "[", False)
init (ForHuman _) = (return (), undefined)


done :: World m => ExecuteMode -> Bool -> m ()
done (ForMachine _) _ = World.putStrLn "]"
done (ForHuman _) _ = return ()


step :: World m => ExecuteMode -> InfoFormatterF a -> StateT Bool m a
step mode infoFormatter =
    case infoFormatter of
        OnInfo info next ->
            logInfo mode info *> return next

        YesOrNo usingStdout prompt next ->
            lift $ putStrLn usingStdout (Text.unpack prompt) *> fmap next World.getYesOrNo

        Empty a ->
            return a


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
            maybe (return ()) json $ jsonInfoMessage elmVersion info

        ForHuman usingStdout ->
            lift $ putStrLn usingStdout (showInfoMessage info)


json :: World m => Json.JSValue -> StateT Bool m ()
json jsvalue =
    do
        printComma <- get
        when printComma (lift $ World.putStr ",")
        lift $ World.putStrLn $ Json.encode jsvalue
        put True
