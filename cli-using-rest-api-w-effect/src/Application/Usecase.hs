{-# LANGUAGE FlexibleContexts #-}
module Application.Usecase (execute) where

import Application.Port
import Domain.Post (PostWithComments)
import Application.Error (AppError(..))
import Domain.Options
import Polysemy (Members, Sem)
import Polysemy.Error (Error, fromEither)
import Polysemy.Async (Async, sequenceConcurrently)
import Data.Maybe (catMaybes)

execute :: Members '[UserDataPort, OutputPort, Logger, Async, Error AppError] r => Options -> Sem r ()
execute options = do
  logInfo "start: use case"
  let uid = options.userId
  posts <- getPosts uid >>= fromEither   -- Either AppError [Post]
  -- Run each getPostWithComments concurrently.
  results <- sequenceConcurrently (getPostWithComments <$> posts)
  let ePwcs = sequence (catMaybes results)
  pwcs <- fromEither ePwcs
  savePostWithCommentsList options.outputPath pwcs >>= fromEither
  logInfo "end: use case"