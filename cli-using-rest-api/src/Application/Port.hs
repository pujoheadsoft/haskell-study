module Application.Port where

import Domain.Post

class Monad m => UserDataPort m where
  getPosts :: UserId -> m [Post]
  getPostWithCommentsList :: [Post] -> m [PostWithComments]

class Monad m => OutputPort m where
  savePostWithCommentsList :: FilePath -> [PostWithComments] -> m ()
