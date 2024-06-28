{-# LANGUAGE BlockArguments #-}
module Gateway.TodoGateway where
import Polysemy (interpret, Sem, Embed, Member, embed)
import Usecase.TodoPort
import Domain.Error
import Domain.Todo hiding (completed, title)
import Domain.User

runTodoGateway :: Member (Embed IO) r => Sem (TodoPort : r) a -> Sem r a
runTodoGateway = interpret $ \case
  FindTodos userId -> embed $ _findTodos userId
  
-- runPort :: forall r. TodoPortType -> Run (TODO_PORT + AFF + r) ~> Run (AFF + r)
-- runPort t run = interpret (on _todoPort (todoPortHandler t) send) run

-- todoPortHandler :: forall r. TodoPortType -> TodoPort ~> Run (AFF + r)
-- todoPortHandler t r = case r of
--   FindTodos userId next -> do
--     todos <- liftAff $ t.findTodos userId
--     pure $ next todos

_findTodos :: UserId -> IO (Either Error Todos)
_findTodos (UserId id) = return $ Right [todo (TodoTitle "") Completed]
  -- do
  -- res <- liftAff $ get string $ "https://jsonplaceholder.typicode.com/users/" <> show id <> "/todos"
  -- case res of
  --   Left err -> do
  --     pure $ Left $ Error $ "GET /api response failed to decode: " <> printError err
  --   Right response -> do
  --     case readJSON response.body of
  --       Right (todos :: TodosJson) -> do
  --         pure $ Right $ todos <#> \(title, completed) -> todo (TodoTitle title) if completed then Completed else InCompleted
  --       Left e -> do
  --         pure $ Left $ Error $ "Can't parse JSON. " <> show e

data TodoJson = TodoJson {
  title :: String,
  completed :: Bool
}

type TodosJson = [TodoJson]