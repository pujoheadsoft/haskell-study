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
{-# LANGUAGE ImpredicativeTypes #-}
module Usecase.DisplayCompletedTodosSpec (spec) where

import Test.Hspec
import Usecase.DisplayCompletedTodos (execute, execute2)
import Domain.User (UserId(UserId))
import Domain.Todo (todo, TodoTitle (TodoTitle), TodoStatus (Completed), Todos, Logics (..))
import Data.Function ((&))
import Polysemy (runM, interpret, Sem)
import Usecase.TodoPort
import Usecase.TodoOutputPort
import Domain.Error (Error(Error))
import Test.HMock (makeMockable, runMockT, ExpectContext (expect), (|->))
import Test.MockCat (createStubFn, (|>), createMock, stubFn, shouldApplyTo)

makeMockable [t|TodoPortClass|]
makeMockable [t|TodoOutputPortClass|]

spec :: Spec
spec = do
  describe "Test DisplayCompletedTodos" $ do
    it "execute" $ do
      let
        todos = [todo (TodoTitle "hoge") Completed]
        completedTodos = [] :: Todos
      f <- createStubFn $ todos |> completedTodos
      findTodosStub <- createStubFn $ UserId 10 |> pure @(Either Error) todos
      setTodosMock <- createMock $ completedTodos |> ()
      
      let
        runTodoGateway :: Sem (TodoPort : r) a -> Sem r a
        runTodoGateway = interpret $ \case
          FindTodos userId -> pure $ findTodosStub userId

        runOutputPort :: Sem (TodoOutputPort : r) a -> Sem r a
        runOutputPort = interpret $ \case
          SetTodos t -> pure $ stubFn setTodosMock t
          SetError _ -> undefined

        l = Logics {
          completed = f
        }

      v <- execute (UserId 10) l
        & runTodoGateway
        & runOutputPort
        & runM

      v `shouldBe` ()
      setTodosMock `shouldApplyTo` completedTodos

    it "execute2 (experiment)" $ do
      let
        todos = [todo (TodoTitle "hoge") Completed]
        completedTodos = [todo (TodoTitle "hoge") Completed]
      f <- createStubFn $ todos |> completedTodos
      example $ do
        runMockT $ do
          expect $ FindTodos2 (UserId 10) |-> Right todos
          expect $ SetTodos2 completedTodos |-> ()
          execute2 (UserId 10) Logics {
            completed = f
          }
