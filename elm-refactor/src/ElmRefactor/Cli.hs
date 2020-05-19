module ElmRefactor.Cli (main) where

import Prelude ()
import ElmRefactor.Prelude

import CommandLine.Program (ProgramIO)
import CommandLine.TransformFiles (TransformMode(..))
import CommandLine.World
import qualified CommandLine.World as World
import Data.Coapplicative
import Data.Either.Extra (collectErrors)
import Data.Text (Text)
import ElmFormat.Upgrade_0_19 (Transformation(..), UpgradeDefinition, parseUpgradeDefinition, applyTransformation)
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


upgrade :: [Transformation] -> (FilePath, Text) -> Either InfoMessage Text
upgrade transformations (inputFile, inputText) =
    let
        elmVersion = Elm_0_19
    in
    case Parse.parse elmVersion inputText of
        Result.Result _ (Result.Ok ast) ->
            let
                transform input =
                    foldl' (flip applyTransformation) input transformations
            in
            ast
                |> fmap (I.convert (Identity . extract))
                |> transform
                |> Render.render elmVersion
                |> Right

        Result.Result _ (Result.Err errs) ->
            Left $ ParseError inputFile errs


main' :: forall m. World m => Flags.Flags -> ProgramIO m ErrorMessage ()
main' flags =
    let
        autoYes = _autoYes flags

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

        let transformations = mconcat
                -- TODO: keep the relative order of these
                [ fmap Upgrade definitions
                , fmap ApplyImport $ Flags._imports flags
                ]

        result <- Program.liftM $ TransformFiles.applyTransformation ProcessingFile autoYes FilesWillBeOverwritten (upgrade transformations) mode
        if result
            then return ()
            else Program.failed


main :: World m => [String] -> m ()
main args =
    Program.run (Flags.parser ElmRefactor.Version.asString) main' args
