{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Application.UsecaseSpec (spec) where

import Test.Hspec
import Domain.Post
import Application.Usecase
import Domain.Options
import Application.Port
import Control.Monad.IO.Unlift  (MonadUnliftIO)
import Test.MockCat (makeMock, runMockT, MockT, (|>))
import Control.Monad.Except (MonadError(..))
import Application.Error (AppError(..))
import Data.List (isInfixOf)
import Control.Exception (ErrorCall(..))

makeMock [t|UserDataPort|]
makeMock [t|OutputPort|]

instance (MonadUnliftIO m) => MonadAsync (MockT m) where
  mapConcurrently = traverse

instance MonadError AppError (MockT IO) where
  throwError e = error (show e)
  catchError action _handler = action

spec :: Spec
spec = describe "usecaseのテスト" do

  it "ユーザーの投稿とコメントをまとめたものを取得して保存する" do
    let 
      post = Post "1" "title" "body"
      comment = Comment "1" "name" "email" "body"
    result <- runMockT do
      _getPosts $ ("1" :: UserId) |> Right [post]
      _getPostWithComments $ post |>
        Right (PostWithComments post [ comment ])
      _savePostWithCommentsList $ ("output.json" :: FilePath)
        |> [ PostWithComments post [ comment ] ]
        |> Right ()
      execute (Options "1" "output.json")
    result `shouldBe` ()

  it "getPosts で DecodeError" do
    runMockT (do
      _getPosts $ ("1" :: UserId) |> Left (DecodeError "bad json")
      execute (Options "1" "out.json")
      ) `shouldThrow` \(ErrorCall s) -> "DecodeError" `isInfixOf` s


  it "getPostWithComments で NetworkError" do
    runMockT (do
      let post = Post "1" "t" "b"
      _getPosts $ ("1" :: UserId) |> Right [post]
      _getPostWithComments $ post |> Left (NetworkError "boom")
      execute (Options "1" "out.json")
      ) `shouldThrow` \(ErrorCall s) -> "NetworkError" `isInfixOf` s
