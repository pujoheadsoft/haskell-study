{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Application.UsecaseSpec where

import Test.Hspec
import Domain.Post
import Application.Usecase
import Domain.Options
import Application.Port
import Control.Monad.IO.Unlift  (MonadUnliftIO)
import Test.MockCat (makeMock, runMockT, MockT, (|>))

makeMock [t|UserDataPort|]
makeMock [t|OutputPort|]

instance (MonadUnliftIO m) => MonadAsync (MockT m) where
  mapConcurrently = traverse

spec :: Spec
spec = do
  describe "usecaseのテスト" do
    it "ユーザーの投稿とコメントをまとめたものを取得して保存する" do
      let 
        post = Post "1" "title" "body"
        comment = Comment "1" "name" "email" "body"

      result <- runMockT do
        _getPosts $ ("1" :: UserId) |> [post]

        _getPostWithCommentsList $ [post] |>
          [ PostWithComments post [ comment ] ]

        _savePostWithCommentsList $ ("output.json" :: FilePath)
          |> [ PostWithComments post [ comment ] ]
          |> ()

        execute (Options "1" "output.json")

      result `shouldBe` ()

    it "ユーザーの投稿とコメントをまとめたものを取得して保存する その2" do
      let 
        post = Post "1" "title" "body"
        comment = Comment "1" "name" "email" "body"

      result <- runMockT do
        _getPosts $ ("1" :: UserId) |> [post]

        _getPostWithComments $ post |>
          PostWithComments post [ comment ]

        _savePostWithCommentsList $ ("output.json" :: FilePath)
          |> [ PostWithComments post [ comment ] ]
          |> ()

        execute2 (Options "1" "output.json")

      result `shouldBe` ()