{-# LANGUAGE Rank2Types #-}
module CommandLine.World.HttpWrapper where

import Prelude ()


newtype HttpWrapper m manager =
    HttpWrapper
        (forall a.
            (manager -> m a)
            -> m a
        )
