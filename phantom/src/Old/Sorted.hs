{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
module Old.Sorted where

import           Data.Coerce     (coerce)
import qualified Data.List       as L
import qualified Data.List.Utils as U
import          Named            (type (~~))
import           The             (The, the)

newtype SortedBy com a = SortedBy a

instance The (SortedBy comp a) a

sortBy
  :: ((a -> a -> Ordering) ~~ comp)
  -> [a]
  -> SortedBy comp [a]
sortBy comp xs = coerce $ L.sortBy (the comp) xs

mergeBy
  :: ((a -> a -> Ordering) ~~ comp)
  -> SortedBy comp [a]
  -> SortedBy comp [a]
  -> SortedBy comp [a]
mergeBy comp xs ys =
  coerce $ U.mergeBy (the comp) (the xs) (the ys)

minimum_01 :: SortedBy comp [a] -> Maybe a
minimum_01 xs = case the xs of
  [] -> Nothing
  x : _ -> Just x