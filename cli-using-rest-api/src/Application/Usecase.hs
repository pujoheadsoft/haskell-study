module Application.Usecase (execute, execute2) where

import Application.Port
import Domain.Options

execute :: (UserDataPort m, OutputPort m) => Options -> m ()
execute options = do
  let uid = options.userId
  posts <- getPosts uid
  postWithComments <- getPostWithCommentsList posts
  savePostWithCommentsList options.outputPath postWithComments

execute2 :: (UserDataPort m, OutputPort m, MonadAsync m) => Options -> m ()
execute2 options = do
  let uid = options.userId
  posts <- getPosts uid
  postWithComments <- xmapConcurrently getPostWithComments posts
  savePostWithCommentsList options.outputPath postWithComments