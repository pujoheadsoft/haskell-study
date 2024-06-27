{-# LANGUAGE TemplateHaskell, LambdaCase, BlockArguments, GADTs
           , FlexibleContexts, TypeOperators, DataKinds, PolyKinds, ScopedTypeVariables #-}

module Usecase.TodoPort where

import Domain.User
import Domain.Error (Error)
import Domain.Todo
import Polysemy (makeSem)

data TodoPort m a where
  FindTodos :: UserId -> TodoPort m (Either Error Todos)

makeSem ''TodoPort