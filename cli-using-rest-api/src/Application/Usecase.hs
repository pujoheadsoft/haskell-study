{-# LANGUAGE FlexibleContexts #-}
module Application.Usecase (execute) where

import Application.Port
import Application.Error (AppError)
import Control.Monad.Except (MonadError, throwError)
import Domain.Options

execute :: (UserDataPort m, OutputPort m, MonadAsync m, MonadError AppError m) => Options -> m ()
execute options = do
  let uid = options.userId
  posts <- getPosts uid >>= liftEither
  eList <- mapConcurrently getPostWithComments posts
  pwcs <- liftEither (sequence eList)
  savePostWithCommentsList options.outputPath pwcs >>= liftEither

liftEither :: MonadError AppError m => Either AppError a -> m a
liftEither = either throwError pure