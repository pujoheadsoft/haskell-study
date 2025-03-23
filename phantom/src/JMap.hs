-- https://github.com/matt-noonan/justified-containers/blob/master/src/Data/Map/Justified.hs
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
module JMap where
import qualified Data.Map as M
import Named (The)

newtype JMap ks k v = JMap (M.Map k v) deriving Functor
newtype Key ks k = Key k

instance The (JMap ks k v) (M.Map k v)
instance The (Key ks k) k

member :: Ord k => k -> JMap ks k v -> Maybe (Key ks k)
member k (JMap m) = (const $ Key k) <$> (M.lookup k m)

lookup :: Ord k => Key ks k -> JMap ks k v -> v
lookup (Key k) (JMap m) = case M.lookup k m of
  Just v -> v
  Nothing -> error "lookup"

reinsert :: Ord k => Key ks k -> v -> JMap ks k v -> JMap ks k v
reinsert (Key k) v = mmap (M.insert k v) 

withMap :: M.Map k v -> (forall ks. JMap ks k v -> t) -> t
withMap m cont = cont (JMap m)

mmap
  :: (M.Map k1 v1 -> M.Map k2 v2)
  -> JMap ks1 k1 v1
  -> JMap ks2 k2 v2
mmap f (JMap m) = JMap (f m)