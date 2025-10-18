{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Application.Port where

import Domain.Post
import Control.Monad.IO.Unlift (MonadUnliftIO)

class Monad m => UserDataPort m where
  getPosts :: UserId -> m [Post]
  getPostWithCommentsList :: [Post] -> m [PostWithComments]
  getPostWithComments :: Post -> m PostWithComments

class Monad m => OutputPort m where
  savePostWithCommentsList :: FilePath -> [PostWithComments] -> m ()

class MonadUnliftIO m => MonadAsync m where
  xmapConcurrently :: Traversable t => (a -> m b) -> t a -> m (t b)