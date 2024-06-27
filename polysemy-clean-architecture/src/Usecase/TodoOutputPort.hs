{-# LANGUAGE TemplateHaskell, LambdaCase, BlockArguments, GADTs
           , FlexibleContexts, TypeOperators, DataKinds, PolyKinds, ScopedTypeVariables #-}

module Usecase.TodoOutputPort where
import Domain.Todo
import Domain.Error
import State.TodoState
import Polysemy

data TodoOutputPortType = TodoOutputPortType {
  stateForTodos :: Todos -> TodoState,
  stateForError :: Error -> TodoState
}

data TodoOutputPort m a where
  SetTodos :: Todos -> TodoOutputPort m a
  SetError :: Error -> TodoOutputPort m a

makeSem ''TodoOutputPort

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