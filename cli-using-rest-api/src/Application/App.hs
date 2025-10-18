{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Application.App (run) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT (runReaderT), MonadReader (ask))

import Application.Port
import Application.Usecase
import Environment
import Domain.Post
import Domain.Options
import Infrastructure.JsonPlaceholderApiDriver
import qualified Infrastructure.JsonPlaceholderApiDriver as Api
import Infrastructure.FileDriver
import qualified Infrastructure.FileDriver as File
import Control.Monad.IO.Unlift  (MonadUnliftIO)
import UnliftIO.Async as UAsync
import Control.Concurrent.Async as Async

run :: IO ()
run = do
  let env = Environment { apiBaseUrl = "https://jsonplaceholder.typicode.com" }
  options <- parseOptions
  runAppM (execute options) env

newtype AppM a = AppM {runAppM :: ReaderT Environment IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader Environment, MonadUnliftIO)

runAppM :: AppM a -> Environment -> IO a
runAppM appM = runReaderT appM.runAppM

instance MonadAsync AppM where
  mapConcurrently = UAsync.mapConcurrently

instance UserDataPort AppM where
  getPosts uid = do
    env <- ask
    postJsons <- fetchPosts env uid
    pure $ toPost <$> postJsons

  getPostWithCommentsList posts = do
    env <- ask
    liftIO $ Async.mapConcurrently (toPostWithComments env) posts
    where
      toPostWithComments env post = do
        commentsJson <- fetchComments env post.id
        let comments = toComment <$> commentsJson
        pure $ PostWithComments post comments

  getPostWithComments post = do
    env <- ask
    commentsJson <- fetchComments env post.id
    let comments = toComment <$> commentsJson
    pure $ PostWithComments post comments

instance OutputPort AppM where
  savePostWithCommentsList path postWithComments = do
    saveAsJson path (PostWithCommentsListJson
      (toPostWithCommentsJson <$> postWithComments))

toPost :: Api.PostJson -> Post
toPost p = Post (show p.id) p.title p.body

toComment :: Api.CommentJson -> Comment
toComment c = Comment (show c.id) c.name c.email c.body

toPostWithCommentsJson :: PostWithComments -> PostWithCommentsJson
toPostWithCommentsJson pwc = PostWithCommentsJson
  { post = File.PostJson pwc.post.title pwc.post.body
  , comments = toCommentJson <$> pwc.comments
  }

toCommentJson :: Comment -> File.CommentJson
toCommentJson c = File.CommentJson c.name c.email c.body