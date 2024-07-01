{-# LANGUAGE TemplateHaskell #-}
module Lib where

import TH
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Control.Monad.State.Class

someFunc :: String
someFunc = "someFunc"

foo :: Bool -> String -> Int
foo b h = 10

bar :: (Eq a, Eq b) => a -> b -> c -> (a -> b -> c)
bar a b c = \a' b' -> do
  if (a == a' && b == b') then c
  else error ""

class Monad m => XMock m where
  hoge :: String -> Int -> m String


data Hoge = Hoge {value :: String}

data Bar m a where
  Bazz :: String -> Int -> Bar m Bool

-- $(do
--    info <- reify 'foo
--    runIO $ print info
--    return [])

$(do
   info <- reify 'foo
   v <- case info of
    (VarI (Name (OccName n) _) t c) -> pure $ show $ functionTypeToList t
    x -> pure $ show x
   runIO $ print v
   return [])
