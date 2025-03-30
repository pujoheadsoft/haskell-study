{-
  Refinement Type (篩型 ふるいがた) の実装
  篩型は、型の要素が想定する述語に関連付けられた型
-}
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

-- a が p という制約を満たしていることを示す型
newtype a ::: p = SuchThat a
infixr 1 :::
type role (:::) nominal nominal

-- The による a' ::: p から a' への変換
instance The a' a => The (a' ::: p) a where
  the (SuchThat x) = the x

-- Proof p（p という制約が成り立つことの証明）を与えることで、通常の a を a ::: p に変換する関数 ...
(...) :: a -> Proof p -> (a ::: p)
x ...proof = coerce x

-- 型 a が p を満たすことを示す型 (satisfiesは満たすことを意味する)
newtype Satisfies (p :: * -> *) a = Satisfies a
type role Satisfies nominal nominal

-- Satisfiesの二項演算子?:  a ? p は 「a は p が成り立つ」 ことを意味する
type a ? p = Satisfies p a
infixr 1 ?

-- The による a ? p から a への変換
instance The (Satisfies p a) a

-- a から a ? p を作る関数
-- これは、a が p を満たすことを示す型を返す
-- `axiom`(Proof p のデフォルト証明)を使っているので、実際成り立つことが証明できているとき使う
assert :: Defining (p ()) => a -> a ? p
assert x = name x (\x' -> unname (x' ...axiom))

-- 名前付きの a ::: p を a ? p に変換する関数
unname :: (a ~~ name ::: p name) -> (a ? p)
unname = coerce . the