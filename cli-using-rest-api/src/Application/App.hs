{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
module Application.App (run) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT (runReaderT), MonadReader (ask))

import Application.Port
import Application.Usecase
import Control.Monad.Except (MonadError (..))
import Control.Exception (try, throwIO)
import Environment
import Domain.Post
import Domain.Options
import Infrastructure.JsonPlaceholderApiDriver
import qualified Infrastructure.JsonPlaceholderApiDriver as Api
import Infrastructure.FileDriver
import qualified Infrastructure.FileDriver as File
import Application.Error (AppError(..))
import Control.Concurrent.Async as Async
import UnliftIO.Async as UAsync
import Control.Monad.IO.Unlift (MonadUnliftIO)

run :: IO ()
run = do
  let env = Environment { apiBaseUrl = "https://jsonplaceholder.typicode.com" }
  options <- parseOptions
  eRes <- (try $ runApp env (execute options)) :: IO (Either AppError ())
  case eRes of
    Left err -> putStrLn $ "Error: " <> show err
    Right () -> pure ()

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
    eJsons <- fetchPosts env uid
    pure $ fmap (map toPost) eJsons

  getPostWithCommentsList posts = do
    env <- ask
    eLists <- liftIO $ Async.mapConcurrently (single env) posts
    pure $ sequence eLists
    where
      single env post = do
        eComments <- fetchComments env post.id
        pure $ fmap (PostWithComments post . map toComment) eComments

  getPostWithComments post = do
    env <- ask
    eComments <- fetchComments env post.id
    pure $ fmap (PostWithComments post . map toComment) eComments

instance OutputPort AppM where
  savePostWithCommentsList path postWithComments = do
    liftIO $ saveAsJson path (PostWithCommentsListJson (toPostWithCommentsJson <$> postWithComments))
    pure (Right ())

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