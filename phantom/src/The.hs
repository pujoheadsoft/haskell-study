{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module The (The(..), pattern The) where
import Data.Coerce (Coercible, coerce)

-- Phantom data を取り除くための型
class The d a | d -> a where
  -- `the`は、dをaに変換する関数
  the :: d -> a

  -- デフォルト実装は、coerceを使って型を変換する
  -- ただし、dとaがcoerce可能であることが前提
  -- もし、dとaがcoerceできない場合は、インスタンスを定義する必要がある
  default the :: Coercible d a => d -> a
  the = coerce

pattern The :: The d a => a -> d
pattern The x <- (the -> x)