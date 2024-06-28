module Presenter.TodoPresenter where

import Polysemy
import Usecase.TodoOutputPort
import Polysemy.State (put, State)
import State.TodoState 
import Domain.Todo
import Domain.Error

runOutputPort :: Member (State TodoState) r => Sem (TodoOutputPort : r) a -> Sem r a
runOutputPort = interpret $ \case
  SetTodos t -> put $ stateForTodos t
  SetError e -> put $ stateForError e

stateForTodos :: Todos -> TodoState
stateForTodos values = TodoState {
  todos = (\(Domain.Todo.Todo (TodoTitle t) _) -> State.TodoState.Todo {State.TodoState.title = t}) <$> values,
  errorMessage = Nothing
}

stateForError :: Error -> TodoState
stateForError (Error e) = TodoState {
  todos = [],
  errorMessage = Just e
}

-- runOutputPort :: forall r. TodoOutputPortType -> Run (TODO_OUTPUT_PORT + STATE TodoState + r) ~> Run (STATE TodoState + r)
-- runOutputPort t run = interpret (on _todoOutputPort (todoOutputHandler t) send) run

-- todoOutputHandler :: forall r. TodoOutputPortType -> TodoOutputPort ~> Run (STATE TodoState + r)
-- todoOutputHandler t port = case port of
--   SetTodos todos next -> do
--     put $ t.stateForTodos todos
--     pure next
--   SetError e next -> do
--     put $ t.stateForError e
--     pure next

-- createOutputPort :: TodoOutputPortType
-- createOutputPort = {
--   stateForTodos: stateForTodos,
--   stateForError: stateForError
-- }