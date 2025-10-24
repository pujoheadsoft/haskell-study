{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Application.Port where

import Domain.Post
import Application.Error (AppError)
import Data.Text (Text)
import Polysemy (makeSem)

data UserDataPort m a where
  GetPosts :: UserId -> UserDataPort m (Either AppError [Post])
  GetPostWithComments :: Post -> UserDataPort m (Either AppError PostWithComments)
makeSem ''UserDataPort

data OutputPort m a where
  SavePostWithCommentsList :: FilePath -> [PostWithComments] -> OutputPort m (Either AppError ())
makeSem ''OutputPort


data Logger m a where
  LogInfo :: Text -> Logger m ()
makeSem ''Logger

-- class Monad m => UserDataPort m where
--   getPosts :: UserId -> m (Either AppError [Post])
--   getPostWithComments :: Post -> m (Either AppError PostWithComments)

-- class Monad m => OutputPort m where
--   savePostWithCommentsList :: FilePath -> [PostWithComments] -> m (Either AppError ())

-- class Monad m => MonadAsync m where
--   mapConcurrently :: Traversable t => (a -> m b) -> t a -> m (t b)

-- class Monad m => Logger m where
--   logInfo :: Text -> m ()