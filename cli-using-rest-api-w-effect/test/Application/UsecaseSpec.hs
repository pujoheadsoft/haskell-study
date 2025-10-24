{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Application.UsecaseSpec (spec) where

import Prelude hiding (any)
import Test.Hspec
import Domain.Post
import Application.Usecase (execute)
import Domain.Options (Options(..))
import Application.Port
import Application.Error (AppError(..))
import Polysemy
import Polysemy.Error
import Polysemy.Async (asyncToIOFinal, Async)
import Test.MockCat (createStubFn, (|>), createMock, stubFn, shouldApplyTo, any, shouldApplyTimes, to)

spec :: Spec
spec = describe "usecaseのテスト (Polysemy effects)" do

  it "ユーザーの投稿とコメントをまとめたものを取得して保存する" do
    let
      post = Post "1" "title" "body"
      postWithComments = PostWithComments post [Comment "101" "name" "email" "comment body"]

    getPost <- createStubFn $ ("userId" :: UserId) |> (Right [post] :: Either AppError [Post])
    getPostWithComments <- createStubFn $ post |> (Right postWithComments :: Either AppError PostWithComments)
    savePostWithCommentsList <- createMock $ ("out.json" :: FilePath) |> [postWithComments] |> (Right () :: Either AppError ())

    let
      runUserDataPort :: Sem (UserDataPort : r) a -> Sem r a
      runUserDataPort = interpret $ \case
        GetPosts uid -> pure $ getPost uid
        GetPostWithComments post -> pure $ getPostWithComments post

      runOutputPort :: Sem (OutputPort : r) a -> Sem r a
      runOutputPort = interpret $ \case
        SavePostWithCommentsList path pwcs -> pure $ stubFn savePostWithCommentsList path pwcs

    r <- runApp
       . runOutputPort
       . runUserDataPort
       $ execute (Options "userId" "out.json")

    savePostWithCommentsList `shouldApplyTo` (("out.json" :: FilePath) |> [postWithComments])
    r `shouldBe` Right ()

  it "getPosts で DecodeError を返して中断" do
    let err = DecodeError "bad json"
    getPostsStub <- createStubFn $ ("userId" :: UserId) |> (Left err :: Either AppError [Post])
    saveStub <- createMock $ ("out.json" :: FilePath) |> ([] :: [PostWithComments]) |> (Right () :: Either AppError ())

    let
      runUserDataPort :: Sem (UserDataPort : r) a -> Sem r a
      runUserDataPort = interpret \case
        GetPosts uid -> pure $ getPostsStub uid
        GetPostWithComments _ -> undefined
      runOutputPort :: Sem (OutputPort : r) a -> Sem r a
      runOutputPort = interpret \case
        SavePostWithCommentsList path pwcs -> pure $ stubFn saveStub path pwcs

    r <- runApp
       . runOutputPort
       . runUserDataPort
       $ execute (Options "userId" "out.json")
    r `shouldBe` Left err

  it "getPostWithComments の途中で NetworkError" do
    let post = Post "1" "title" "body"
        err = NetworkError "boom"
    getPostsStub <- createStubFn $ ("userId" :: UserId) |> (Right [post] :: Either AppError [Post])
    getPostWithCommentsStub <- createStubFn $ post |> (Left err :: Either AppError PostWithComments)
    saveStub <- createMock $ any @FilePath |> any @[PostWithComments] |> (Right () :: Either AppError ())

    let
      runUserDataPort :: Sem (UserDataPort : r) a -> Sem r a
      runUserDataPort = interpret \case
        GetPosts uid -> pure $ getPostsStub uid
        GetPostWithComments p -> pure $ getPostWithCommentsStub p

      runOutputPort :: Sem (OutputPort : r) a -> Sem r a
      runOutputPort = interpret \case
        SavePostWithCommentsList path pwcs -> pure $ stubFn saveStub path pwcs

    r <- runApp
       . runOutputPort
       . runUserDataPort
       $ execute (Options "userId" "out.json")

    r `shouldBe` Left err
    saveStub `shouldApplyTimes` (0 :: Int) `to` (any @FilePath |> any @[PostWithComments])

runApp :: Sem [Error AppError, Embed IO, Async, Logger, Final IO] a -> IO (Either AppError a)
runApp = runFinal
     . runLogger
     . asyncToIOFinal
     . embedToFinal
     . runError @AppError

runLogger :: Sem (Logger : r) a -> Sem r a
runLogger = interpret $ \case
  LogInfo _ -> pure ()
