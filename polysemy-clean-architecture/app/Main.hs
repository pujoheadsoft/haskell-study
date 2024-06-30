{-# LANGUAGE TemplateHaskell #-}
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
import Mock

main :: IO ()
main = do
  v <- Usecase.execute (UserId 10) logics
    & runTodoGateway
    & runOutputPort
    & runState (TodoState {todos = [], errorMessage = Nothing})
    & runM
  putStr (show (fst v))

funA :: String -> String
funA a = a

mockFunA :: String -> String
mockFunA a = do
  if a == "a" then a
  else error "not a"

-- main :: IO ()
-- main = do
--   argsAndTypes <- runQ (getFunctionArgsAndTypes 'funA)
--   putStrLn "Function arguments and their types:"
--   mapM_ print argsAndTypes

