{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Domain.Todo where

newtype TodoTitle = TodoTitle String deriving (Show, Eq)

data TodoStatus = Completed | InCompleted deriving (Show, Eq)

data Todo = Todo {
  title :: TodoTitle,
  status :: TodoStatus
} deriving (Show, Eq)

todo :: TodoTitle -> TodoStatus -> Todo
todo title status = Todo {title, status}

type Todos = [Todo]

_completed :: Todos -> Todos
_completed = filter \(Todo _ status) -> status == Completed

data Logics = Logics {
  completed :: Todos -> Todos
}

logics :: Logics
logics = Logics {
  completed = _completed
}


