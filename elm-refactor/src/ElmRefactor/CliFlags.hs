module ElmRefactor.CliFlags (Flags(..), parser) where

import Prelude ()
import Relude hiding (stdin)

import AST.V0_16
import qualified AST.Listing
import qualified AST.Module
import qualified Options.Applicative as Opt
import qualified Text.PrettyPrint.ANSI.Leijen as PP


data Flags = Flags
    { _autoYes :: Bool
    , _upgradeDefinitions :: [FilePath]
    , _imports :: [AST.Module.UserImport]
    , _input :: [FilePath]
    }


parser :: String -> Opt.ParserInfo Flags
parser elmRefactorVersion =
    Opt.info
        (Opt.helper <*> flags)
        (helpInfo elmRefactorVersion)



-- COMMANDS

flags :: Opt.Parser Flags
flags =
    Flags
      <$> yes
      <*> upgradeDefinition
      <*> Opt.many importDefinition
      <*> Opt.some input


-- HELP

helpInfo :: String -> Opt.InfoMod Flags
helpInfo elmRefactorVersion =
    mconcat
        [ Opt.fullDesc
        , Opt.headerDoc $ Just top
        , Opt.progDesc "Refactor Elm source files."
        , Opt.footerDoc (Just examples)
        ]
  where
    top =
        PP.vcat
            [ PP.text $ "elm-refactor " ++ elmRefactorVersion
            ]

    examples =
        linesToDoc
        [ "You may specify multiple transformations, and they will be applied in order."
        , ""
        , "Examples:"
        , "  elm-refactor --upgrade Upgrade.elm Main.elm  # refactors Main.elm"
        , "  elm-refactor --upgrade Upgrade.elm src/      # refactors all *.elm files in the src directory"
        , "  elm-refactor --import 'Html.Attributes as Attr' --import 'Dict exposing (Dict)' ./"
        , "                                               # applies the given imports to all *.elm files in the current directory"
        -- , ""
        -- , "Full guide to using elm-refactor at <https://github.com/avh4/elm-format>"
        ]


linesToDoc :: [String] -> PP.Doc
linesToDoc lineList =
    PP.vcat (map PP.text lineList)


upgradeDefinition :: Opt.Parser [FilePath]
upgradeDefinition =
    Opt.many $ Opt.strOption $
        mconcat
        [ Opt.long "upgrade"
        , Opt.metavar "UPGRADE_DEFINITION"
        , Opt.help "Transform files using the upgrade transformation in the given file."
        ]

importDefinition :: Opt.Parser AST.Module.UserImport
importDefinition =
    -- TODO: parse the string: https://stackoverflow.com/questions/46182591/parsing-enum-options-with-optparse-applicative
    fmap (\_ -> (C [] $ fmap UppercaseIdentifier ["Html", "Attributes"], AST.Module.ImportMethod (Just $ C ([], []) $ UppercaseIdentifier "Attr") (C ([], []) AST.Listing.ClosedListing)))
    $ Opt.strOption $ mconcat
        [ Opt.long "import"
        , Opt.metavar "IMPORT_METHOD"
        , Opt.helpDoc $ Just $ mconcat
              [ PP.text "Transform files to import the module as given.", PP.line
              , PP.text "Examples:", PP.line
              , PP.indent 2 $ mconcat
                  [ PP.text "--import 'Html.Attributes as Attributes'", PP.line
                  , PP.text "--import 'Html.Attributes exposing (href)'"
                  ]
              ]
        ]


input :: Opt.Parser FilePath
input =
    Opt.strArgument $
        mconcat
        [ Opt.metavar "INPUT"
        , Opt.help "The .elm file to transform. You may specify a directory, which will be searched recursively. You may specify more than one."
        ]


yes :: Opt.Parser Bool
yes =
    Opt.switch $
        mconcat
        [ Opt.long "yes"
        , Opt.help "Reply 'yes' to all automated prompts."
        ]
