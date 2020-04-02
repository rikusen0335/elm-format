import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import Control.Monad (forM_)

import qualified Shakefiles.Haskell
import qualified Shakefiles.Shellcheck
import qualified Shakefiles.Dependencies


main :: IO ()
main = do
    shakefilesHash <- getHashedShakeVersion [ "Shakefile.hs" ]
    shakeArgs shakeOptions{
      shakeChange = ChangeModtimeAndDigest,
      shakeColor = True,
      shakeVersion = shakefilesHash
    } $ do
    StdoutTrim stackLocalInstallRoot <- liftIO $ cmd "stack path --local-install-root"
    StdoutTrim gitDescribe <- liftIO $ cmd "git" [ "describe", "--abbrev=8", "--always" ]
    StdoutTrim gitSha <- liftIO $ cmd "git" [ "describe", "--always", "--match", "NOT A TAG", "--dirty" ]

    let elmFormat = stackLocalInstallRoot </> "bin/elm-format" <.> exe
    let elmRefactor = stackLocalInstallRoot </> "bin/elm-refactor" <.> exe

    shellcheck <- Shakefiles.Dependencies.rules

    want [ "test" ]

    phony "test" $ do
        need
            [ "stack-test"
            , "integration-tests"
            , "shellcheck"
            ]

    phony "build" $ need [ "elm-format", "elm-refactor" ]
    phony "elm-format" $ need [ elmFormat ]
    phony "elm-refactor" $ need [ elmRefactor ]
    phony "stack-test" $ need
        [ "_build/stack/elm-format-lib/test.ok"
        , "_build/stack/elm-format-test-lib/test.ok"
        , "_build/stack/elm-format/test.ok"
        , "_build/stack/elm-refactor/test.ok"
        ]
    phony "profile" $ need [ "_build/tests/test-files/prof.ok" ]

    phony "clean" $ do
        cmd_ "stack clean"
        removeFilesAfter "_build" [ "//*" ]
        removeFilesAfter ""
            [ "_input.elm"
            , "_input2.elm"
            , "formatted.elm"
            , "formatted.json"
            , "_stdout.txt"
            ]


    --
    -- build
    --

    "generated/Build_elm_format.hs" %> \out -> do
        alwaysRerun
        writeFileChanged out $ unlines
            [ "module Build_elm_format where"
            , ""
            , "gitDescribe :: String"
            , "gitDescribe = " ++ show (gitDescribe :: String)
            ]

    Shakefiles.Haskell.cabalProject "elm-format-markdown"
        [ "elm-format-markdown/elm-format-markdown.cabal"
        , "elm-format-markdown/src//*.hs"
        , "elm-format-markdown//*.hs"
        ]
        [] [] []

    Shakefiles.Haskell.cabalProject "elm-format-lib"
        [ "elm-format-lib/elm-format-lib.cabal"
        , "elm-format-lib/src//*.hs"
        ]
        [ "elm-format-markdown" ]
        [] []

    Shakefiles.Haskell.cabalProject "elm-format-test-lib"
        [ "elm-format-test-lib/elm-format-test-lib.cabal"
        , "elm-format-test-lib/src//*.hs"
        ]
        [ "elm-format-lib" ]
        [ "elm-format-test-lib/test//*.hs" ]
        []

    Shakefiles.Haskell.cabalProject "elm-format"
        [ "elm-format.cabal"
        , "generated/Build_elm_format.hs"
        , "src//*.hs"
        ]
        [ "elm-format-lib" ]
        [ "tests//*.hs"
        , "tests//*.stdout"
        , "tests//*.stderr"
        ]
        [ "elm-format-test-lib" ]

    Shakefiles.Haskell.exe elmFormat "elm-format"

    Shakefiles.Haskell.cabalProject "elm-refactor"
        [ "elm-refactor/elm-refactor.cabal"
        , "elm-refactor/src//*.hs"
        ]
        [ "elm-format-lib" ]
        [ "elm-refactor/test//*.hs"
        , "elm-refactor/test//*.stdout"
        , "elm-refactor/test//*.stderr"
        ]
        [ "elm-format-test-lib" ]

    Shakefiles.Haskell.exe elmRefactor "elm-refactor"


    --
    -- integration tests
    --

    phony "integration-tests" $ do
        need
            [ "_build/run-tests.ok"
            , "_build/tests/test-files/good-json.ok"
            , "_build/tests/test-files/good/Elm-0.19.ok"
            , "_build/tests/test-files/good/Elm-0.18.ok"
            , "_build/tests/test-files/good/Elm-0.17.ok"
            , "_build/tests/test-files/transform/Elm-0.19.ok"
            , "_build/tests/test-files/transform/Elm-0.18.ok"
            , "_build/tests/test-files/transform/Elm-0.17.ok"
            , "_build/tests/test-files/upgrade/Elm-0.19.ok"
            , "_build/tests/test-files/upgrade/Elm-0.18.ok"
            , "_build/tests/test-files/upgrade/Elm-0.17.ok"
            , "_build/tests/test-files/bad/Elm-0.19.ok"
            , "_build/tests/test-files/bad/Elm-0.18.ok"
            , "_build/tests/test-files/bad/Elm-0.17.ok"
            , "_build/tests/test-files/elm-refactor/Elm-0.19.ok"
            ]

    "_build/run-tests.ok" %> \out -> do
        let script = "tests/run-tests.sh"
        need [ script, elmFormat ]
        testFiles <- getDirectoryFiles ""
            [ "tests/test-files/good/json//*.elm"
            , "tests/test-files/bad//*.elm"
            , "tests/test-files/directory//*.elm"
            , "tests/test-files/recursive-directory//*.elm"
            , "tests/test-files/*.json"
            ]
        need testFiles
        cmd_ ("bash" <.> exe) script elmFormat
        writeFile' out ""

    "_build/tests/test-files/prof.ok" %> \out -> do
        let oks =
              [ "_build/tests/test-files/good/Elm-0.17/prof.ok"
              , "_build/tests/test-files/good/Elm-0.18/prof.ok"
              , "_build/tests/test-files/good/Elm-0.19/prof.ok"
              ]
        need oks
        writeFile' out (unlines oks)


    -- Elm files

    let elmVersions = [ "0.17", "0.18", "0.19" ]

    forM_ elmVersions $ \elmVersion -> do
        ("_build/tests/test-files/good/Elm-" ++ elmVersion ++ ".ok") %> \out -> do
            elmFiles <- getDirectoryFiles ""
                [ "tests/test-files/good/Elm-" ++ elmVersion ++ "//*.elm"
                ]
            let oks = ["_build" </> f -<.> "elm_matches" | f <- elmFiles]
            need oks
            writeFile' out (unlines elmFiles)

        ("_build/tests/test-files/good/Elm-" ++ elmVersion ++ "/prof.ok") %> \out -> do
            alwaysRerun
            elmFiles <- getDirectoryFiles ""
                [ "tests/test-files/good/Elm-" ++ elmVersion ++ "//*.elm"
                ]
            let oks = ["_profile" </> f -<.> (gitSha ++ ".prof") | f <- elmFiles]
            need oks
            writeFile' out (unlines oks)

        ("_profile/tests/test-files/good/Elm-" ++ elmVersion ++ "//*." ++ gitSha ++ ".prof") %> \out -> do
            let source = dropDirectory1 $ dropExtensions out <.> "elm"
            need [ "_build/bin/elm-format-prof", source ]
            cmd_ "_build/bin/elm-format-prof" source "--output" "/dev/null" ("--elm-version=" ++ elmVersion) "+RTS" "-p" ("-po" ++ (out -<.> ""))

        ("_build/tests/test-files/*/Elm-" ++ elmVersion ++ "//*.elm_formatted") %> \out -> do
            let source = dropDirectory1 $ out -<.> "elm"
            need [ elmFormat, source ]
            cmd_ elmFormat source "--output" out ("--elm-version=" ++ elmVersion)

        ("_build/tests/test-files/*/Elm-" ++ elmVersion ++ "//*.elm_upgraded") %> \out -> do
            let source = dropDirectory1 $ out -<.> "elm"
            need [ elmFormat, source ]
            cmd_ elmFormat source "--output" out "--upgrade" ("--elm-version=" ++ elmVersion)

        ("_build/tests/test-files/*/Elm-" ++ elmVersion ++ "//*.elm_stderr") %> \out -> do
            let source = dropDirectory1 $ out -<.> "elm"
            need [ elmFormat, source ]
            (Stderr stderr, Exit _) <- cmd (FileStdin source) BinaryPipes elmFormat "--stdin" ("--elm-version=" ++ elmVersion)
            cmd_ (FileStdout out) (Stdin stderr) BinaryPipes "tr" [ "-d", "\r" ]

        ("_build/tests/test-files/transform/Elm-" ++ elmVersion ++ ".ok") %> \out -> do
            elmFiles <- getDirectoryFiles ""
                [ "tests/test-files/transform/Elm-" ++ elmVersion ++ "//*.elm"
                ]
            let oks = ["_build" </> f -<.> "elm_transform_matches" | f <- elmFiles, ( takeExtension $ dropExtension f) /= ".formatted" ]
            need oks
            writeFile' out (unlines oks)

        ("_build/tests/test-files/upgrade/Elm-" ++ elmVersion ++ ".ok") %> \out -> do
            elmFiles <- getDirectoryFiles ""
                [ "tests/test-files/upgrade/Elm-" ++ elmVersion ++ "//*.elm"
                ]
            let oks = ["_build" </> f -<.> "elm_upgrade_matches" | f <- elmFiles, ( takeExtension $ dropExtension f) /= ".formatted" ]
            need oks
            writeFile' out (unlines oks)

        ("_build/tests/test-files/bad/Elm-" ++ elmVersion ++ ".ok") %> \out -> do
            elmFiles <- getDirectoryFiles ""
                [ "tests/test-files/bad/Elm-" ++ elmVersion ++ "//*.elm"
                ]
            let oks = ["_build" </> f -<.> "elm_bad_matches" | f <- elmFiles ]
            need oks
            writeFile' out (unlines elmFiles)

        ("_build/tests/test-files/elm-refactor/Elm-" ++ elmVersion ++ ".ok") %> \out -> do
            upgradeFiles <- getDirectoryFiles ""
                [ "tests/test-files/elm-refactor/Elm-" ++ elmVersion ++ "//*.upgrade_elm"
                ]
            let oks = ["_build" </> f -<.> "elm_refactor_matches" | f <- upgradeFiles ]
            need oks
            writeFile' out (unlines upgradeFiles)

    "_build/tests//*.elm_matches" %> \out -> do
        let actual = out -<.> "elm_formatted"
        let expected = dropDirectory1 $ out -<.> "elm"
        need [ actual, expected ]
        cmd_ "diff" "-u" actual expected
        writeFile' out ""

    "_build/tests//*.elm_transform_matches" %> \out -> do
        let actual = out -<.> "elm_formatted"
        let expected = dropDirectory1 $ out -<.> "formatted.elm"
        need [ actual, expected ]
        cmd_ "diff" "-u" actual expected
        writeFile' out ""

    "_build/tests//*.elm_upgrade_matches" %> \out -> do
        let actual = out -<.> "elm_upgraded"
        let expected = dropDirectory1 $ out -<.> "formatted.elm"
        need [ actual, expected ]
        cmd_ "diff" "-u" actual expected
        writeFile' out ""

    "_build/tests//*.elm_bad_matches" %> \out -> do
        let actual = out -<.> "elm_stderr"
        let expected = dropDirectory1 $ out -<.> "output.txt"
        need [ actual, expected ]
        cmd_ "diff" "--strip-trailing-cr" "-u" actual expected
        writeFile' out ""

    "_build/tests/test-files/elm-refactor/Elm-0.19//*.elm_refactor_upgraded" %> \out -> do
        let source = dropDirectory1 $ out -<.> "elm"
        let upgradeDefinition = dropDirectory1 $ out -<.> "upgrade_elm"
        need [ elmRefactor, source, upgradeDefinition ]
        cmd_ "cp" source out
        cmd_ elmRefactor "--yes" "--upgrade" upgradeDefinition out

    "_build/tests//*.elm_refactor_matches" %> \out -> do
        let actual = out -<.> "elm_refactor_upgraded"
        let expected = dropDirectory1 $ out -<.> "output.elm"
        need [ actual, expected ]
        cmd_ "diff" "--strip-trailing-cr" "-u" actual expected
        writeFile' out ""


    -- JSON files

    "_build/tests/test-files/good-json.ok" %> \out -> do
        jsonFiles <- getDirectoryFiles ""
            [ "tests/test-files/good//*.json"
            ]
        let oks = ["_build" </> f -<.> "json_matches" | f <- jsonFiles]
        need oks
        writeFile' out (unlines jsonFiles)

    "_build/tests//*.json_formatted" %> \out -> do
        let source = dropDirectory1 $ out -<.> "elm"
        need [ elmFormat, source ]
        (Stdout rawJson) <- cmd (FileStdin source) elmFormat "--elm-version=0.19" "--stdin" "--json"
        (Stdout formattedJson) <- cmd (Stdin rawJson) "python3" "-mjson.tool" "--sort-keys"
        writeFileChanged out formattedJson

    "_build/tests//*.json_matches" %> \out -> do
        let actual = out -<.> "json_formatted"
        let expected = dropDirectory1 $ out -<.> "json"
        need [ actual, expected ]
        cmd_ "diff" "--strip-trailing-cr" "-u" actual expected
        writeFile' out ""


    Shakefiles.Shellcheck.rules shellcheck
