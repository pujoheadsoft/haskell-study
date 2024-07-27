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
import State.TodoState
import Usecase.DisplayCompletedTodos (execute, execute2)
import Domain.User (UserId(UserId))
import Domain.Todo (logics, todo, TodoTitle (TodoTitle), TodoStatus (Completed), Todo (Todo), Todos, Logics (..))
import Polysemy.State (runState, put, State)
import Data.Function ((&))
import Polysemy (runM, interpret, embed, Member, Sem, Embed)
import Usecase.TodoPort
import Usecase.TodoOutputPort
import Domain.Error (Error(Error))
import Test.HMock (makeMockable, runMockT, ExpectContext (expectAny, expect), (|->))
import Test.MockCat (createStubFn, (|>), createMock, stubFn, shouldApplyTo)
import Control.Monad.Trans

makeMockable [t|TodoPortClass|]
makeMockable [t|TodoOutputPortClass|]

spec :: Spec
spec = do
  describe "Test DisplayCompletedTodos" $ do
    it "execute" $ do
      let
        todos = [todo (TodoTitle "hoge") Completed]
        completedTodos = [todo (TodoTitle "hoge") Completed]
      findFn <- createStubFn $ todos |> completedTodos

      xx <- createStubFn $ UserId 10 |> (pure (Right todos) :: Sem [TodoOutputPort, Embed IO] (Either Error Todos))
      yy <- createMock $ completedTodos |> (pure () :: Sem '[Embed IO] ())
      
      let
        yf = stubFn yy
        --runTodoGateway :: Sem (TodoPort : r) a -> Sem r a
        runTodoGateway = interpret $ \case
          FindTodos userId -> xx userId

        --runOutputPort :: Member (State TodoState) r => Sem (TodoOutputPort : r) a -> Sem r a
        runOutputPort = interpret $ \case
          SetTodos t -> yf t
          SetError _ -> undefined

      v <- execute (UserId 10) logics
        & runTodoGateway
        & runOutputPort
        & runM

      v `shouldBe` ()
      yy `shouldApplyTo` completedTodos

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

{-
    it "" $ do
      let
        f = mock $ do
          "a" :> "b" :> true
          "b" :> "a" :> false 

        completed_fn = mock $ do
          [(todo (TodoTitle "hoge") Completed)] :> [(todo (TodoTitle "hoge") Completed)]
      l = Logics {
        completed = completed_fn
      }

      runMockT $ do
        stub findTodos2 (UserId 10) :> Right [todo (TodoTitle "hoge") Completed]
        stub any :> ()

        execute2 (UserId 10) l

        verify setTodos2 [todo (TodoTitle "hoge") Completed]

        
-}

