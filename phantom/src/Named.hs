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

-- 「a に名前を付けて、その名前付き値で続きの処理をする」関数
name :: a -> (forall name. (a ~~ name) -> t) -> t
name x k = k (coerce x)

instance The (Named name a) a

data Defn = Defn

-- 制約の同義語で、`p`が定義されているモジュール内でのみ利用可能であることが期待される
-- 型はエクスポートされ、コンストラクタは非公開とする
type Defining p = (Coercible p Defn, Coercible Defn p)

-- 定義した名前に関する規則を定義できるようにする
-- coerceは、この関数がNamedモジュール内にあるため可能
defn :: Defining f => a -> (a ~~ f)
defn = coerce
