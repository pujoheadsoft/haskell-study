{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Application.Port where

import Domain.Post
import Application.Error (AppError)

class Monad m => UserDataPort m where
  getPosts :: UserId -> m (Either AppError [Post])
  getPostWithCommentsList :: [Post] -> m (Either AppError [PostWithComments])
  getPostWithComments :: Post -> m (Either AppError PostWithComments)

class Monad m => OutputPort m where
  savePostWithCommentsList :: FilePath -> [PostWithComments] -> m (Either AppError ())

class Monad m => MonadAsync m where
  mapConcurrently :: Traversable t => (a -> m b) -> t a -> m (t b)