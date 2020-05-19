{-# LANGUAGE OverloadedStrings #-}
module ElmCompiler.Reporting.Report
    ( Report(..)
    )
    where


import qualified ElmCompiler.Reporting.Annotation as A
import qualified ElmCompiler.Reporting.Doc as D



-- BUILD REPORTS


data Report =
  Report
    { _title :: String
    , _region :: A.Region
    , _sgstns :: [String]
    , _message :: D.Doc
    }
