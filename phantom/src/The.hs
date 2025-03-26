{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module The (The(..), pattern The) where
import Data.Coerce (Coercible, coerce)

class The d a | d -> a where
  the :: d -> a
  default the :: Coercible d a => d -> a
  the = coerce

pattern The :: The d a => a -> d
pattern The x <- (the -> x)