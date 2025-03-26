{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wno-star-is-type #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Refined where
import The (The (the))
import Named (Defining, type (~~), name)
import Proof (Proof, axiom)
import Data.Coerce (coerce)

newtype a ::: p = SuchThat a
infixr 1 :::
type role (:::) nominal nominal

instance The a' a => The (a' ::: p) a where
  the (SuchThat x) = the x

(...) :: a -> Proof p -> (a ::: p)
x ...proof = coerce x

newtype Satisfies (p :: * -> *) a = Satisfies a
type role Satisfies nominal nominal

instance The (Satisfies p a) a

type a ?p = Satisfies p a
infixr 1 ?

assert :: Defining (p ()) => a -> a ?p
assert x = name x (\x' -> unname (x' ...axiom))

unname :: (a ~~ name ::: p name) -> (a ?p)
unname = coerce . the