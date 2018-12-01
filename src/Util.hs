{-# LANGUAGE NoImplicitPrelude #-}
module Util where

import Data.Function (flip)
import Data.Functor (Functor, (<$>))

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)
