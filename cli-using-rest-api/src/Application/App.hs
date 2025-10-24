{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Application.App (run) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT (runReaderT), MonadReader (ask))

import Application.Port
import Application.Usecase
import Control.Monad.Except (MonadError (..))
import Control.Exception (catch, throwIO, try)
import Environment
import Domain.Post
import Domain.Options
import qualified Infrastructure.JsonPlaceholderApiDriver as Api
import qualified Infrastructure.FileDriver as File
import Application.Error (AppError(..))
import UnliftIO.Async as UAsync
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Optics (_Right, traversed, (%), (%~))

run :: IO ()
run = do
  options <- parseOptions
  env <- loadEnvironment
  runApp env (execute options)
    `catch` \(err :: AppError) -> putStrLn $ "Error: " <> show err

newtype AppM a = AppM (ReaderT Environment IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader Environment, MonadUnliftIO)

runApp :: Environment -> AppM a -> IO a
runApp env (AppM r) = runReaderT r env

instance MonadError AppError AppM where
  throwError e = AppM $ liftIO (throwIO e)
  catchError (AppM r) handler = AppM $ do
    env <- ask
    er <- liftIO $ try (runReaderT r env)
    either (\e -> case handler e of AppM r' -> r') pure er

instance MonadAsync AppM where
  mapConcurrently = UAsync.mapConcurrently

instance UserDataPort AppM where
  getPosts uid = do
    env <- ask
    eJsons <- Api.fetchPosts env uid
    pure $ (_Right % traversed %~ toPost) eJsons

  getPostWithComments post = do
    env <- ask
    eCommentJsons <- Api.fetchComments env post.id
    pure $ PostWithComments post <$> (_Right % traversed %~ toComment) eCommentJsons

instance OutputPort AppM where
  savePostWithCommentsList path postWithComments = do
    liftIO $ File.saveAsJson path (File.PostWithCommentsListJson (toPostWithCommentsJson <$> postWithComments))
    pure (Right ())

instance Logger AppM where
  logInfo msg = liftIO $ print msg

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