{-# LANGUAGE TemplateHaskell, LambdaCase, BlockArguments, GADTs
           , FlexibleContexts, TypeOperators, DataKinds, PolyKinds, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Usecase.TodoPort where

import Domain.User
import Domain.Error (Error)
import Domain.Todo
import Polysemy (makeSem)

-- data TodoPortType = TodoPortType {
--   _findTodos :: UserId -> Either Error Todos
-- }

data TodoPort m a where
  FindTodos :: UserId -> TodoPort m (Either Error Todos)

makeSem ''TodoPort

class Monad m => TodoPortClass m where
  findTodos2 :: UserId -> m (Either Error Todos)