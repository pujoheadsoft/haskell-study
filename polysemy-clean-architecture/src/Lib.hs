{-# LANGUAGE TemplateHaskell #-}
module Lib where

import TH
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Control.Monad.State.Class
import Data.IORef (IORef, newIORef, modifyIORef, writeIORef, readIORef)
import GHC.IO (unsafePerformIO)
import GHC.IORef (IORef(IORef))

someFunc :: String
someFunc = "someFunc"

foo :: Bool -> String -> Int
foo b h = 10

data Rec a = Rec { callValue :: a }

bar :: (Eq a, Eq b) => a -> b -> c -> (a -> b -> c, IORef (Rec a))
bar a b c = do
  let
    ref = unsafePerformIO $ newIORef Rec { callValue = a }
    fn a' b' = do
      let _ = unsafePerformIO $ writeIORef ref (Rec { callValue = a })
      if a == a' && b == b' then c
      else error ""
  (fn, ref)

zz :: String -> (String, IO (IORef (Rec String)))
zz s = ("", newIORef Rec {callValue = ""})

z2 = do
  let
    (fn, x) = bar "X1" "X2" (0 :: Integer)
    r = fn "X1" "X2"
  (Rec a) <- readIORef x
  print r
  print a


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

barlo :: (Eq a, Eq b) => a -> b -> c -> IO (a -> b -> c, IORef (Rec a))
barlo a b c = do
  ref <- newIORef Rec { callValue = a }
  let
    fn a' b' = do
      let _ = writeIORef ref (Rec { callValue = a })
      if a == a' && b == b' then c
      else error ""
  pure (fn, ref)

z3 = do
  (fn, x) <- barlo "X1" "X2" (0 :: Integer)
  let
    r = fn "X1" "X2"
  (Rec a) <- readIORef x
  print r
  print a