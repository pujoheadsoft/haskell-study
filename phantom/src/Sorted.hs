{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE RoleAnnotations #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Sorted where

import qualified Data.List       as L
import          Named            (type (~~), Defn)
import           The
import Refined                   (type (?), assert)

newtype SortedBy com name = SortedBy Defn
type role SortedBy nominal nominal

sortBy
  :: ((a -> a -> Ordering) ~~ comp)
  -> [a]
  -> ([a] ?SortedBy comp)
sortBy (The comp) xs = assert $ L.sortBy comp xs

mergeBy
  :: ((a -> a -> Ordering) ~~ comp)
  -> ([a] ?SortedBy comp)
  -> ([a] ?SortedBy comp)
  -> ([a] ?SortedBy comp)
mergeBy (The comp) (The xs) (The ys) =
  assert $ unsafeMergeBy comp xs ys

-- minimum_01 :: SortedBy comp [a] -> Maybe a
-- minimum_01 xs = case the xs of
--   [] -> Nothing
--   x : _ -> Just x

unsafeMergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
unsafeMergeBy comp = go
  where
    go [] ys' = ys'
    go xs' [] = xs'
    go (x : xs') (y : ys') = case comp x y of
      GT -> y : go (x : xs') ys'
      _  -> x : go xs' (y : ys')