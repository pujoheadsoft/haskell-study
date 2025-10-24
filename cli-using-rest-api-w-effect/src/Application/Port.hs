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
