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
import Polysemy (Embed, embed)
import Polysemy.Async (Async, asyncToIOFinal)
import Polysemy.Final (runFinal, embedToFinal)

run :: IO ()
run = do
  options <- parseOptions
  env <- loadEnvironment
  e <- runSem env (execute options)
  either (\err -> putStrLn $ "Error: " <> show err) (const (pure ())) e

-- Effect stack includes Async from polysemy-async interpreted via asyncToIOFinal.
-- We use final style to support real concurrency.
-- NOTE: Order of effects matters. We place Embed IO BEFORE Async so that
-- we can remove Embed IO (embedToFinal) producing stack [Async, Final IO]
-- which asyncToIOFinal then interprets. UserDataPort needs Reader & Embed IO.
runSem :: Environment -> Sem '[UserDataPort, Reader Environment, OutputPort, Logger, Error AppError, Embed IO, Async, Final IO] a -> IO (Either AppError a)
runSem env = runFinal
          . asyncToIOFinal            -- interpret Async after Final IO is introduced by embedToFinal (to its right)
          . embedToFinal              -- eliminate Embed IO, introduce Final IO
          . runError @AppError        -- handle errors
          . runLoggerIO               -- interpret Logger (needs Embed IO)
          . runOutputPortIO           -- interpret OutputPort (needs Embed IO)
          . runReader env             -- keep Reader available for runUserDataPortIO (executed next)
          . runUserDataPortIO         -- interpret UserDataPort (needs Reader & Embed IO)

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

-- (Removed custom MonadAsync interpreter; using polysemy-async's asyncToIOFinal instead.)