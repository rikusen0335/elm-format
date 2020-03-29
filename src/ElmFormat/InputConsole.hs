module ElmFormat.InputConsole (InputConsole, InputConsoleF(..), readStdin, execute) where

import CommandLine.World (World)
import qualified CommandLine.World as World
import Control.Monad.Free
import Data.Text (Text)


class Functor f => InputConsole f where
    readStdin :: f Text


data InputConsoleF a
    = ReadStdin (Text -> a)
    deriving (Functor)


instance InputConsole InputConsoleF where
    readStdin = ReadStdin id


instance InputConsole f => InputConsole (Free f) where
    readStdin = liftF readStdin


execute :: World m => InputConsoleF a -> m a
execute operation =
    case operation of
        ReadStdin next ->
            next <$> World.getStdin
