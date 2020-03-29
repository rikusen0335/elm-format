module ElmRefactor.Messages (PromptMessage(..), InfoMessage(..), ErrorMessage(..)) where

import qualified CommandLine.ResolveFiles as ResolveFiles
import qualified Data.Text as Text
import ElmFormat.InfoFormatter (ToConsole(..), Loggable(..))
import qualified ElmRefactor.Version
import qualified Text.JSON as Json
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Syntax as Syntax
import Reporting.Region (Region(..), Position(..))


data PromptMessage
    = FilesWillBeOverwritten [FilePath]


showFiles :: [FilePath] -> Text
showFiles = unlines . fmap (\filename -> "    " <> Text.pack filename)


instance ToConsole PromptMessage where
    toConsole = \case
        FilesWillBeOverwritten filePaths ->
            unlines
                [ "This will overwrite the following files:"
                , ""
                , showFiles filePaths
                , "This cannot be undone! Make sure to back up these files before proceeding."
                , ""
                , "Are you sure you want to overwrite these files? (y/n)"
                ]


data InfoMessage
  = ProcessingFile FilePath
  | FileWouldChange FilePath
  | ParseError FilePath String [A.Located Syntax.Error]


instance ToConsole InfoMessage where
    toConsole = \case
        ProcessingFile file ->
            "Processing file " <> Text.pack file

        FileWouldChange file ->
            "File would be changed " <> Text.pack file

        ParseError inputFile _ errs ->
            let
                location =
                    Text.pack $
                    case errs of
                        [] -> inputFile
                        (A.A (Region (Position line col) _) _) : _ -> inputFile ++ ":" ++ show line ++ ":" ++ show col
            in
            "Unable to parse file " <> location <> " To see a detailed explanation, run elm make on the file."


instance Loggable InfoMessage where
    jsonInfoMessage elmVersion =
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
                "File would be changed by elm-refactor-" <> ElmRefactor.Version.asString
                -- TODO: add arguments that were passed to elm-refactor
                <> " --elm-version=" <> show elmVersion -- TODO: elm-refactor doesn't yet support --elm-version
        ParseError inputFile _ _ ->
            Just $ fileMessage inputFile "Error parsing the file"


data ErrorMessage
    = BadInputFiles [ResolveFiles.Error]
    | BadUpgradeDefinitions [FilePath]


instance ToConsole ErrorMessage where
    toConsole = \case
        BadInputFiles filePaths ->
            unlines
                [ "There was a problem reading one or more of the specified INPUT paths:"
                , ""
                , unlines $ fmap ((<>) "    " . toConsole) filePaths
                , "Please check the given paths."
                ]

        BadUpgradeDefinitions filePaths ->
            unlines
                [ "There was a problem reading one or more of the specified UPGRDATE_DEFINITION files:"
                ,""
                , unlines $ fmap (\file -> "    " <> Text.pack file <> ": Failed to parse upgrade definition. It is not valid Elm syntax.") filePaths
                ]
