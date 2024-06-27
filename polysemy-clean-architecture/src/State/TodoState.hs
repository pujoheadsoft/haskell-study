module State.TodoState where

data Todo = Todo { title :: String }

data TodoState = TodoState { 
  todos :: [Todo],
  errorMessage :: Maybe String
}