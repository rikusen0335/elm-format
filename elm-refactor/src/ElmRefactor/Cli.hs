module ElmRefactor.Cli (main) where

import Elm.Utils ((|>))

import CommandLine.Program (ProgramIO)
import CommandLine.TransformFiles (TransformMode(..))
import Control.Monad.Free
import Data.Coapplicative
import Data.Text (Text)
import ElmFormat.InfoFormatter (ExecuteMode(..), approve)
import ElmFormat.Upgrade_0_19 (UpgradeDefinition, parseUpgradeDefinition, transformModule)
import ElmFormat.World
import ElmRefactor.CliFlags as Flags
import ElmRefactor.Messages
import ElmVersion

import qualified CommandLine.Program as Program
import qualified CommandLine.TransformFiles as TransformFiles
import qualified Data.Indexed as I
import qualified ElmFormat.FileStore as FileStore
import qualified ElmFormat.Operation as Operation
import qualified ElmFormat.Parse as Parse
import qualified ElmFormat.Render.Text as Render
import qualified ElmRefactor.Version
import qualified Reporting.Result as Result


upgrade :: [UpgradeDefinition] -> (FilePath, Text) -> Either InfoMessage Text
upgrade upgradeDefinitions (_, inputText) =
    let
        elmVersion = Elm_0_19
    in
    case Parse.parse elmVersion inputText of
        Result.Result _ (Result.Ok ast) ->
            let
                transform input =
                    foldl' (flip transformModule) input upgradeDefinitions
            in
            ast
                |> fmap (I.convert (Identity . extract))
                |> transform
                |> Render.render elmVersion
                |> Right

        Result.Result _ (Result.Err _ ) ->
            -- TODO: return an error message
            error "TODO: couldn't parse source file"

main' :: World m => Flags.Flags -> ProgramIO m String ()
main' flags =
    let
        autoYes = True

        readDefinitionFile definitionFile =
            Program.liftME
                $ fmap (first (\() -> "Failed to parse upgrade definition"))
                $ parseUpgradeDefinition . snd <$> foldFree Operation.execute (FileStore.readFileWithPath definitionFile)
    in
    do
        mode <- case Flags._input flags of
            [] -> Program.showUsage
            first:rest -> return $ FilesInPlace first rest

        let definitionFiles = Flags._upgradeDefinitions flags
        definitions <- mapM readDefinitionFile definitionFiles

        result <- Program.liftM $ TransformFiles.applyTransformation ProcessingFile (approve (ForHuman undefined) autoYes . showPromptMessage . FilesWillBeOverwritten) (upgrade definitions) mode
        if result
            then return ()
            else Program.failed


main :: World m => [String] -> m ()
main args =
    Program.run (Flags.parser ElmRefactor.Version.asString) id main' args
