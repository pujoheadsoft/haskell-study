{-# LANGUAGE FlexibleContexts #-}
module Application.Usecase (execute) where

import Application.Port
import Application.Error (AppError(..))
import Domain.Options
import Polysemy (Members, Sem)
import Polysemy.Error (Error, fromEither)
import Polysemy.Async (Async, sequenceConcurrently)
import Data.Maybe (catMaybes)

execute :: Members '[UserDataPort, OutputPort, Logger, Async, Error AppError] r => Options -> Sem r ()
execute (Options userId outputPath) = do
  logInfo "start: use case"
  let posts = getPosts userId >>= fromEither
  pwcs <- posts
      >>= sequenceConcurrently . map getPostWithComments
      >>= fromEither . sequence . catMaybes
  savePostWithCommentsList outputPath pwcs >>= fromEither
  logInfo "end: use case"