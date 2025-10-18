{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Application.UsecaseSpec where

import Test.Hspec
import Domain.Post
import Application.Usecase
import Domain.Options
import Application.Port
import Test.MockCat (makeMock, runMockT, (|>))

makeMock [t|UserDataPort|]
makeMock [t|OutputPort|]

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