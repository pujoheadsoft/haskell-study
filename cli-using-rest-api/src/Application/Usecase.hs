module Application.Usecase (execute) where

import Application.Port
import Domain.Options

execute :: (UserDataPort m, OutputPort m) => Options -> m ()
execute options = do
  let uid = options.userId
  posts <- getPosts uid
  postWithComments <- getPostWithCommentsList posts
  savePostWithCommentsList options.outputPath postWithComments