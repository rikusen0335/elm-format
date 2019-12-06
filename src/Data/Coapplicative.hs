module Data.Coapplicative (Coapplicative(..)) where

import Data.Functor.Identity


class Functor f => Coapplicative f where
    extract :: f a -> a

instance Coapplicative ((,) x) where
    extract (_, a) = a
    {-# INLINE extract #-}

instance Coapplicative Identity where
    extract = runIdentity
    {-# INLINE extract #-}
