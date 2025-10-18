module Application.Usecase (execute, execute2) where

import Application.Port
import Domain.Options

-- 非同期処理を型クラスで抽象化していないバージョン
-- 非同期でやるとは表明していない
-- 使う側は非同期にしようが同期にしようがどっちでもよい
execute :: (UserDataPort m, OutputPort m) => Options -> m ()
execute options = do
  let uid = options.userId
  posts <- getPosts uid
  postWithComments <- getPostWithCommentsList posts
  savePostWithCommentsList options.outputPath postWithComments

-- 非同期処理も型クラスで抽象化したバージョン
-- 非同期でやることを明示している
-- 使う側は非同期に対応する必要がある(実装では同期にすることも可能だが)
execute2 :: (UserDataPort m, OutputPort m, MonadAsync m) => Options -> m ()
execute2 options = do
  let uid = options.userId
  posts <- getPosts uid
  postWithComments <- mapConcurrently getPostWithComments posts
  savePostWithCommentsList options.outputPath postWithComments