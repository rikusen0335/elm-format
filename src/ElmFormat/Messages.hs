module ElmFormat.Messages (PromptMessage(..), showPromptMessage, InfoMessage(..), showInfoMessage, jsonInfoMessage, ErrorMessage(..), showErrorMessage) where

-- inspired by:
-- https://wiki.haskell.org/Internationalization_of_Haskell_programs_using_Haskell_data_types

import Prelude ()
import Relude

import CommandLine.ResolveFiles (ResolveFileError(..))
import Data.Text (Text)
import qualified Data.Text as Text
import ElmFormat.InfoFormatter (Loggable(..))
import qualified ElmFormat.Version
import ElmVersion
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Syntax as Syntax
import Reporting.Region (Region(..), Position(..))
import qualified Text.JSON as Json


data InfoMessage
  = ProcessingFile FilePath
  | FileWouldChange FilePath
  | ParseError FilePath String [A.Located Syntax.Error]


data PromptMessage
    = FilesWillBeOverwritten [FilePath]


data ErrorMessage
  = BadInputFiles [ResolveFileError]
  | NoInputs
  | SingleOutputWithMultipleInputs
  | TooManyInputs
  | OutputAndValidate
  | MustSpecifyVersionWithUpgrade ElmVersion


showFiles :: [FilePath] -> Text
showFiles = unlines . fmap (\filename -> "    " <> Text.pack filename)


showPromptMessage :: PromptMessage -> Text

showPromptMessage (FilesWillBeOverwritten filePaths) =
    unlines
        [ "This will overwrite the following files to use Elm's preferred style:"
        , ""
        , showFiles filePaths
        , "This cannot be undone! Make sure to back up these files before proceeding."
        , ""
        , "Are you sure you want to overwrite these files with formatted versions? (y/n)"
        ]


instance Loggable InfoMessage where
    showInfoMessage (ProcessingFile file) =
        "Processing file " <> Text.pack file

    showInfoMessage (FileWouldChange file) =
        "File would be changed " <> Text.pack file

    showInfoMessage (ParseError inputFile _ errs) =
        let
            location =
                Text.pack $
                case errs of
                    [] -> inputFile
                    (A.A (Region (Position line col) _) _) : _ -> inputFile ++ ":" ++ show line ++ ":" ++ show col
        in
        "Unable to parse file " <> location <> " To see a detailed explanation, run elm make on the file."


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
                "File is not formatted with elm-format-" <> ElmFormat.Version.asString
                <> " --elm-version=" <> show elmVersion
        ParseError inputFile _ _ ->
            Just $ fileMessage inputFile "Error parsing the file"


showErrorMessage :: ErrorMessage -> Text

showErrorMessage (BadInputFiles filePaths) =
  unlines
    [ "There was a problem reading one or more of the specified INPUT paths:"
    , ""
    , unlines $ map ((<>) "    " . showInputMessage) filePaths
    , "Please check the given paths."
    ]

showErrorMessage SingleOutputWithMultipleInputs =
  unlines
    [ "Can't write to the OUTPUT path, because multiple .elm files have been specified."
    , ""
    , "Please remove the --output argument. The .elm files in INPUT will be formatted in place."
    ]

showErrorMessage TooManyInputs =
    "Too many input sources! Please only provide one of either INPUT or --stdin"

showErrorMessage OutputAndValidate =
    "Cannot use --output and --validate together"

showErrorMessage (MustSpecifyVersionWithUpgrade elmVersion) =
    "I can only upgrade code to specific Elm versions.  To make sure I'm doing what you expect, you must also specify --elm-version=" <> Text.pack (show elmVersion) <> " when you use --upgrade."

showErrorMessage NoInputs =
    error "Error case NoInputs should be handled elsewhere.  Please report this issue at https://github.com/avh4/elm-format/issues"


showInputMessage :: ResolveFileError -> Text
showInputMessage (FileDoesNotExist path) =
    Text.pack path <> ": No such file or directory"
showInputMessage (NoElmFiles path) =
    Text.pack path <> ": Directory does not contain any *.elm files"
