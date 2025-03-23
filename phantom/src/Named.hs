{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Named
  ( Named
  , type (~~)
  , name
  , Defining
  , Defn
  , defn
  )
where

import Data.Coerce (coerce, Coercible)
import The (The)

newtype Named name a = Named a
type a ~~ name = Named name a

name :: a -> (forall name. (a ~~ name) -> t) -> t
name x k = k (coerce x)

instance The (Named name a) a

data Defn = Defn

type Defining p = (Coercible p Defn, Coercible Defn p)

defn :: Defining f => a -> (a ~~ f)
defn = coerce
