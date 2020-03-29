module ElmFormat.OutputConsole (OutputConsole, OutputConsoleF(..), writeStdout, execute) where

import CommandLine.World (World)
import qualified CommandLine.World as World
import Control.Monad.Free
import Data.Text (Text)


class Functor f => OutputConsole f where
    writeStdout :: Text -> f ()


data OutputConsoleF a
    = WriteStdout Text a
    deriving (Functor)


instance OutputConsole OutputConsoleF where
    writeStdout content = WriteStdout content ()


instance OutputConsole f => OutputConsole (Free f) where
    writeStdout content = liftF (writeStdout content)


execute :: World m => OutputConsoleF a -> m a
execute operation =
    case operation of
        WriteStdout content next ->
            World.writeStdout content *> return next
