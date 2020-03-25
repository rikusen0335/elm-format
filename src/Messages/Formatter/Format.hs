module Messages.Formatter.Format
    ( InfoFormatter, InfoFormatterF(..), onInfo, approve
    , ExecuteMode(..), init, done, step
    ) where

import Prelude hiding (init)

import Control.Monad.Free
import Control.Monad.State
import ElmVersion (ElmVersion)
import qualified ElmFormat.Version
import ElmFormat.World (World)
import qualified ElmFormat.World as World
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
    = ForMachine


init :: World m => ExecuteMode -> (m (), Bool)
init ForMachine = (World.putStr "[", False)


done :: World m => ExecuteMode -> Bool -> m ()
done ForMachine _ = World.putStrLn "]"


step :: World m => ExecuteMode -> ElmVersion -> Bool -> InfoFormatterF a -> StateT Bool m a
step ForMachine elmVersion autoYes infoFormatter =
    case infoFormatter of
        OnInfo (ProcessingFile _) next -> return next
        OnInfo (FileWouldChange file) next ->
            json next file $
                "File is not formatted with elm-format-" ++ ElmFormat.Version.asString
                ++ " --elm-version=" ++ show elmVersion
        OnInfo (ParseError inputFile _ _) next ->
            json next inputFile "Error parsing the file"

        Approve _prompt next ->
            case autoYes of
                True -> return (next True)
                False -> return (next False)


json :: World m => a -> FilePath -> String -> StateT Bool m a
json next file message =
    do
        printComma <- get
        when printComma (lift $ World.putStr ",")
        lift $ World.putStrLn $ Json.encode $ Json.makeObj
            [ ( "path", Json.JSString $ Json.toJSString file )
            , ( "message", Json.JSString $ Json.toJSString message )
            ]
        put True
        return next
