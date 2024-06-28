module Main (main) where

import Usecase.DisplayCompletedTodos as Usecase
import Domain.User (UserId(UserId))
import Domain.Todo (logics)
import Gateway.TodoGateway (runTodoGateway)
import Presenter.TodoPresenter (runOutputPort)
import Polysemy.State (runState)
import State.TodoState
import Polysemy
import Data.Function ((&))

main :: IO ()
main = do
  v <- Usecase.execute (UserId 10) logics
    & runTodoGateway
    & runOutputPort
    & runState (TodoState {todos = [], errorMessage = Nothing})
    & runM
  putStr (show (fst v))
