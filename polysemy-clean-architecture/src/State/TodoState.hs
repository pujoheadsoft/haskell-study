module State.TodoState where

import Prelude as P
data Todo = Todo { title :: String } deriving (Show, Eq)

data TodoState = TodoState { 
  todos :: [Todo],
  errorMessage :: Maybe String
} deriving (Eq)

instance Show TodoState where
  show t = P.show (errorMessage t)