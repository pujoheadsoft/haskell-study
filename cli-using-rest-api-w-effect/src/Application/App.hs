{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Application.App (run) where

import Application.Port
import Application.Usecase (execute)
import Application.Error (AppError(..))
import Domain.Options
import Domain.Post
import Environment
import qualified Infrastructure.JsonPlaceholderApiDriver as Api
import qualified Infrastructure.FileDriver as File
import Optics (_Right, traversed, (%), (%~))
import Polysemy hiding (run)
import Polysemy.Error (Error, runError)
import Polysemy.Reader (Reader, runReader, ask)
import Polysemy.Async (Async, asyncToIOFinal)

run :: IO ()
run = do
  options <- parseOptions
  env <- loadEnvironment
  e <- runSem env (execute options)
  either (\err -> putStrLn $ "Error: " <> show err) (const (pure ())) e

runSem :: Environment
       -> Sem '[ UserDataPort
         , Reader Environment
         , OutputPort
         , Logger
         , Error AppError
         , Embed IO
         , Async
         , Final IO
         ] a
       -> IO (Either AppError a)
runSem env = runFinal
          . asyncToIOFinal
          . embedToFinal
          . runError @AppError
          . runLoggerIO
          . runOutputPortIO
          . runReader env
          . runUserDataPortIO

-- Interpreters
runLoggerIO :: Member (Embed IO) r => Sem (Logger ': r) a -> Sem r a
runLoggerIO = interpret \case
  LogInfo msg -> embed (print msg)

runOutputPortIO :: Member (Embed IO) r => Sem (OutputPort ': r) a -> Sem r a
runOutputPortIO = interpret \case
  SavePostWithCommentsList path pwcs -> do
    embed $ File.saveAsJson path (File.PostWithCommentsListJson (toPostWithCommentsJson <$> pwcs))
    pure (Right ())

runUserDataPortIO :: Members '[Embed IO, Reader Environment] r => Sem (UserDataPort ': r) a -> Sem r a
runUserDataPortIO = interpret \case
  GetPosts uid -> do
    env <- ask
    eJsons <- embed $ Api.fetchPosts env uid
    pure $ (_Right % traversed %~ toPost) eJsons
  GetPostWithComments post -> do
    env <- ask
    eCommentJsons <- embed $ Api.fetchComments env post.id
    pure $ PostWithComments post <$> (_Right % traversed %~ toComment) eCommentJsons

-- Conversion helpers
toPost :: Api.PostJson -> Post
toPost p = Post (show p.id) p.title p.body

toComment :: Api.CommentJson -> Comment
toComment c = Comment (show c.id) c.name c.email c.body

toPostWithCommentsJson :: PostWithComments -> File.PostWithCommentsJson
toPostWithCommentsJson pwc = File.PostWithCommentsJson
  { File.post = File.PostJson pwc.post.title pwc.post.body
  , File.comments = toCommentJson <$> pwc.comments
  }

toCommentJson :: Comment -> File.CommentJson
toCommentJson c = File.CommentJson c.name c.email c.body
