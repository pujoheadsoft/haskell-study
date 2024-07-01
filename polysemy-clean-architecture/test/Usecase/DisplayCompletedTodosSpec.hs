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
module Usecase.DisplayCompletedTodosSpec (spec) where

import Test.Hspec
import State.TodoState
import Usecase.DisplayCompletedTodos (execute, execute2)
import Domain.User (UserId(UserId))
import Domain.Todo (logics, todo, TodoTitle (TodoTitle), TodoStatus (Completed), Todo (Todo))
import Polysemy.State (runState, put, State)
import Data.Function ((&))
import Polysemy (runM, interpret, embed, Member, Sem, Embed)
import Usecase.TodoPort
import Usecase.TodoOutputPort
import Domain.Error (Error(Error))
import Test.HMock (makeMockable, runMockT, ExpectContext (expectAny, expect), (|->))

makeMockable [t|TodoPortClass|]
makeMockable [t|TodoOutputPortClass|]

spec :: Spec
spec = do
  describe "Test DisplayCompletedTodos" $ do
    it "execute" $ do
      let
        runTodoGateway :: Sem (TodoPort : r) a -> Sem r a
        runTodoGateway = interpret $ \case
          FindTodos userId -> return $ Right [todo (TodoTitle "hoge") Completed]

        runOutputPort :: Member (State TodoState) r => Sem (TodoOutputPort : r) a -> Sem r a
        runOutputPort = interpret $ \case
          SetTodos t -> put TodoState {
              todos = (\(Domain.Todo.Todo (TodoTitle t) _) -> State.TodoState.Todo { title = t }) <$> t,
              errorMessage = Nothing
            }
          SetError (Error e) -> put TodoState {
              todos = [],
              errorMessage = Just e
            }

      v <- execute (UserId 10) logics
        & runTodoGateway
        & runOutputPort
        & runState (TodoState {todos = [], errorMessage = Nothing})
        & runM

      fst v `shouldBe` (TodoState {
        todos = [State.TodoState.Todo { title = "hoge" }],
        errorMessage = Nothing
      })

    it "execute2 (experiment)" $ do
      example $ do
        runMockT $ do
          expect $ FindTodos2 (UserId 10) |-> Right [todo (TodoTitle "hoge") Completed]
          expect $ SetTodos2 [todo (TodoTitle "hoge") Completed] |-> ()
          execute2 (UserId 10) logics

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

