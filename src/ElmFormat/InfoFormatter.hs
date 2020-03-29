{-# LANGUAGE LambdaCase #-}
module ElmFormat.InfoFormatter
    ( Loggable(..), onInfo, approve
    , InfoFormatter(..), InfoFormatterF(..), execute
    , ExecuteMode(..), init, done
    ) where

import Prelude hiding (init, putStrLn)

import Control.Monad.Free
import Control.Monad.State
import Data.Text (Text)
import qualified Data.Text as Text
import ElmVersion (ElmVersion)
import ElmFormat.World (World)
import qualified ElmFormat.World as World
import qualified Text.JSON as Json


class Loggable a where
    showInfoMessage :: a -> Text
    jsonInfoMessage :: ElmVersion -> a -> Maybe Json.JSValue -- TODO: remove ElmVersion


onInfo :: (Monad f, InfoFormatter f, Loggable info) => ExecuteMode -> info -> StateT Bool f ()
onInfo mode info =
    case mode of
        ForMachine elmVersion ->
            maybe (lift $ empty ()) json $ jsonInfoMessage elmVersion info

        ForHuman usingStdout ->
            lift $ putStrLn' usingStdout (showInfoMessage info)


approve :: (Monad f, InfoFormatter f) => ExecuteMode -> Bool -> Text -> f Bool
approve mode autoYes prompt =
    case autoYes of
        True -> empty True

        False ->
            case mode of
                ForMachine _ -> empty False

                ForHuman usingStdout ->
                    putStrLn' usingStdout prompt *> yesOrNo


class Functor f => InfoFormatter f where
    putInfoToStderr :: Text -> f () -- with trailing newline
    putInfoToStdout :: Text -> f () -- with trailing newline
    putInfoToStdoutN :: Text -> f () -- without trailing newline
    yesOrNo :: f Bool
    empty :: a -> f a


data InfoFormatterF a
    = PutInfoToStderr Text a
    | PutInfoToStdout Text a
    | YesOrNo (Bool -> a)
    | Empty a
    deriving (Functor)


instance InfoFormatter InfoFormatterF where
    putInfoToStderr text = PutInfoToStderr (text <> "\n") ()
    putInfoToStdout text = PutInfoToStdout (text <> "\n") ()
    putInfoToStdoutN text = PutInfoToStdout text ()
    yesOrNo = YesOrNo id
    empty value = Empty value


instance InfoFormatter f => InfoFormatter (Free f) where
    putInfoToStderr text = liftF (putInfoToStderr text)
    putInfoToStdout text = liftF (putInfoToStdout text)
    putInfoToStdoutN text = liftF (putInfoToStdoutN text)
    yesOrNo = liftF yesOrNo
    empty value = liftF (empty value)


data ExecuteMode
    = ForMachine ElmVersion
    | ForHuman { _usingStdout :: Bool }


init :: InfoFormatter f => ExecuteMode -> (f (), Bool)
init (ForMachine _) = (putInfoToStdoutN "[", False)
init (ForHuman _) = (empty (), undefined)


done :: InfoFormatter f => ExecuteMode -> Bool -> f ()
done (ForMachine _) _ = putInfoToStdout "]"
done (ForHuman _) _ = empty ()


execute :: World m => InfoFormatterF a -> m a
execute = \case
    PutInfoToStderr text next ->
        World.putStrStderr (Text.unpack text) *> return next

    PutInfoToStdout text next ->
        World.putStr (Text.unpack text) *> return next

    YesOrNo next ->
        fmap next World.getYesOrNo

    Empty a ->
        return a


putStrLn' :: InfoFormatter f => Bool -> Text -> f ()
putStrLn' usingStdout =
    -- we log to stdout unless it is being used for file output (in that case, we log to stderr)
    case usingStdout of
        True -> putInfoToStderr
        False -> putInfoToStdout


json :: (Monad f, InfoFormatter f) => Json.JSValue -> StateT Bool f ()
json jsvalue =
    do
        printComma <- get
        when printComma (lift $ putInfoToStdoutN ",")
        lift $ putInfoToStdout $ Text.pack $ Json.encode jsvalue
        put True
