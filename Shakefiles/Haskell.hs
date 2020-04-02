module Shakefiles.Haskell (cabalProject, Shakefiles.Haskell.exe) where

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util


cabalProject :: String -> [String] -> [String] -> [String] -> [String] -> [String] -> Rules ()
cabalProject name sourceFiles sourcePatterns deps testPatterns testDeps =
    do
        "_build/stack/" </> name </> "build.ok" %> \out -> do
            sourceFilesFromPatterns <- getDirectoryFiles "" sourcePatterns
            let allFiles = mconcat
                    [ [ "stack.yaml" ]
                    , fmap (\d -> "_build/stack" </> d </> "build.ok") deps
                    , sourceFiles
                    , sourceFilesFromPatterns
                    ]
            need allFiles
            hash <- liftIO $ getHashedShakeVersion allFiles
            cmd_ "stack" "build" name
            writeFile' out hash

        "_build/stack/" </> name </> "test.ok" %> \out -> do
            need [ "stack.yaml" ]
            need $ fmap (\d -> "_build/stack" </> d </> "build.ok") deps
            need $ fmap (\d -> "_build/stack" </> d </> "build.ok") testDeps
            need sourceFiles
            sourceFilesFromPatterns <- getDirectoryFiles "" sourcePatterns
            need sourceFilesFromPatterns
            testFiles <- getDirectoryFiles "" testPatterns
            need testFiles
            cmd_ "stack" "test" name "--test-arguments=--hide-successes"
            writeFile' out ""

        "_build/bin" </> name ++ "-prof" %> \out -> do
            StdoutTrim profileInstallRoot <- liftIO $ cmd "stack path --profile --local-install-root" -- TODO: move this to an Oracle
            sourceFiles <- getDirectoryFiles "" sourcePatterns
            cmd_ "stack" "build" "--profile" "--executable-profiling" "--library-profiling"
            copyFileChanged (profileInstallRoot </> "bin" </> name <.> Development.Shake.FilePath.exe) out


exe :: FilePath -> String -> Rules ()
exe target projectName =
    do
        target %> \out -> do
            need [ "_build/stack" </> projectName </> "build.ok" ]
