{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Lib where

import Usecase.DisplayCompletedTodos (execute2)
import Domain.User (UserId(UserId))
import Domain.Todo
import Usecase.TodoPort
import Usecase.TodoOutputPort
import Test.HMock
import Data.IORef
import GHC.IO

makeMockable [t|TodoPortClass|]
makeMockable [t|TodoOutputPortClass|]


{-# NOINLINE logRef #-}
logRef :: IORef [String]
logRef = unsafePerformIO $ newIORef []

{-# NOINLINE __completed #-}
__completed :: (Todos -> Todos, IORef [String])
__completed = unsafePerformIO $ do
  modifyIORef logRef (\log -> "called" : log)
  return (_completed, logRef)

z :: IO ()
z = do
  let
    (f, ref) = __completed
    _logics = Logics {
      completed = f
    }

  runMockT $ do
    expect $ FindTodos2 (UserId 10) |-> Right [todo (TodoTitle "hoge") Completed]
    expect $ SetTodos2 [todo (TodoTitle "hoge") Completed] |-> ()
    execute2 (UserId 10) _logics

  log <- readIORef ref
  print log