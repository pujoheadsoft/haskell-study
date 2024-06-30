module Usecase.DisplayCompletedTodos where
import Domain.User
import Domain.Todo (Logics, completed)
import Polysemy (Sem, Members)
import Usecase.TodoPort
import Usecase.TodoOutputPort

execute
  :: Members '[TodoPort, TodoOutputPort] r
  => UserId
  -> Logics
  -> Sem r ()
execute userId logics = do
  result <- findTodos userId
  case result of
    Right todos -> setTodos $ completed logics todos
    Left e -> setError e

execute2
  :: (TodoPortClass m, TodoOutputPortClass m)
  => UserId
  -> Logics
  -> m ()
execute2 userId logics = do
  result <- findTodos2 userId
  case result of
    Right todos -> setTodos2 $ completed logics todos
    Left e -> setError2 e