-- TODO: move to elm-format, and combine with other Prelude.hs in this repo
module Prelude
    ( module Relude
    , module Elm.Utils
    ) where

import Relude hiding ((>>), exitSuccess, exitFailure)
import Elm.Utils hiding (run)
