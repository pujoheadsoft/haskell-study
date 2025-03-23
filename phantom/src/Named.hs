{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Named where

import Data.Coerce (coerce, Coercible)

newtype Named name a = Named a
type a ~~ name = Named name a

name :: a -> (forall name. (a ~~ name) -> t) -> t
name x k = k (coerce x)

class The d a | d -> a where
  the :: d -> a
  default the :: Coercible d a => d -> a
  the = coerce

instance The (a ~~ name) a