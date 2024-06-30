module Usecase.DisplayCompletedTodosSpec (spec) where

import Test.Hspec
import State.TodoState
import Usecase.DisplayCompletedTodos (execute)
import Domain.User (UserId(UserId))
import Domain.Todo (logics, todo, TodoTitle (TodoTitle), TodoStatus (Completed), Todo (Todo))
import Polysemy.State (runState, put, State)
import Data.Function ((&))
import Polysemy (runM, interpret, embed, Member, Sem, Embed)
import Usecase.TodoPort (TodoPort(FindTodos))
import Usecase.TodoOutputPort (TodoOutputPort(..))
import Domain.Error (Error(Error))

spec :: Spec
spec = do
  describe "Test DisplayCompletedTodos" $ do
    it "execute" $ do
      let
        runTodoGateway :: Member (Embed IO) r => Sem (TodoPort : r) a -> Sem r a
        runTodoGateway = interpret $ \case
          FindTodos userId -> embed $ return $ Right [todo (TodoTitle "hoge") Completed]

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
