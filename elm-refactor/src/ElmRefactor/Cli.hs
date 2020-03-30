module ElmRefactor.Cli (main) where

import Elm.Utils ((|>))

import CommandLine.Program (ProgramIO)
import CommandLine.TransformFiles (TransformMode(..))
import CommandLine.World
import qualified CommandLine.World as World
import Data.Coapplicative
import Data.Either.Extra (collectErrors)
import Data.Text (Text)
import ElmFormat.Upgrade_0_19 (UpgradeDefinition, parseUpgradeDefinition, transformModule)
import ElmRefactor.CliFlags as Flags
import ElmRefactor.Messages
import ElmVersion

import qualified CommandLine.Program as Program
import qualified CommandLine.ResolveFiles as ResolveFiles
import qualified CommandLine.TransformFiles as TransformFiles
import qualified Data.Indexed as I
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

main' :: forall m. World m => Flags.Flags -> ProgramIO m ErrorMessage ()
main' flags =
    let
        autoYes = True

        readDefinitionFile :: FilePath -> m (Either FilePath UpgradeDefinition)
        readDefinitionFile definitionFile =
            fmap (first $ \() -> definitionFile)
                $ parseUpgradeDefinition . snd <$> World.readUtf8FileWithPath definitionFile
    in
    do
        resolvedInputFiles <- Program.mapError BadInputFiles $ Program.liftME $ ResolveFiles.resolveElmFiles (Flags._input flags)
        mode <- case resolvedInputFiles of
            [] -> Program.showUsage
            first:rest -> return $ FilesInPlace first rest

        let definitionFiles = Flags._upgradeDefinitions flags
        definitions <- Program.mapError BadUpgradeDefinitions $ Program.liftME $ collectErrors <$> mapM readDefinitionFile definitionFiles

        result <- Program.liftM $ TransformFiles.applyTransformation ProcessingFile autoYes FilesWillBeOverwritten (upgrade definitions) mode
        if result
            then return ()
            else Program.failed


main :: World m => [String] -> m ()
main args =
    Program.run (Flags.parser ElmRefactor.Version.asString) main' args
