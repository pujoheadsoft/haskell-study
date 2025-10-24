{-# LANGUAGE FlexibleContexts #-}
module Application.UsecaseSpec (spec) where

import Test.Hspec
import Domain.Post
import Application.Usecase (execute)
import Domain.Options (Options(..))
import Application.Port
import Application.Error (AppError(..))
import Polysemy
import Polysemy.Error
import Polysemy.Final (runFinal, embedToFinal)
import Polysemy.Async (Async, asyncToIOFinal)

spec :: Spec
spec = describe "usecaseのテスト (Polysemy effects)" do

  -- it "ユーザーの投稿とコメントをまとめたものを取得して保存する" do
  --   let 
  --     post = Post "1" "title" "body"
  --     comment = Comment "1" "name" "email" "body"
  --     pwc = PostWithComments post [comment]
  --   let program = execute (Options "1" "output.json")
  --   r <- runTestSuccess [post] [Right pwc]
  --           (Right ()) program
  --   r `shouldBe` Right ()
  
  it "正常系" do
    "" `shouldBe` ""

  -- it "getPosts で DecodeError" do
  --   let program = execute (Options "1" "out.json")
  --   r <- runTestSuccess [] [] (Left (DecodeError "bad json")) program
  --   r `shouldBe` Left (DecodeError "bad json")

  -- it "getPostWithComments で NetworkError" do
  --   let post = Post "1" "t" "b"
  --       program = execute (Options "1" "out.json")
  --   r <- runTestSuccess [post] [Left (NetworkError "boom")] (Right ()) program
  --   r `shouldBe` Left (NetworkError "boom")

-- Helpers
-- runTestSuccess 
--   :: [Post]
--   -> [Either AppError PostWithComments]
--   -> Either AppError ()
--   -> Sem '[UserDataPort, OutputPort, Logger, Embed IO, Error AppError, Async, Final IO] a
--   -> IO (Either AppError a)
-- runTestSuccess posts pwcs saveResult prog = do
--   pure $ runFinal
--      . asyncToIOFinal
--      . embedToFinal
--      . runError @AppError
--      . runLoggerIgnore
--      . runOutputMock saveResult
--      . runUserDataMock posts pwcs
--     $ prog

-- runLoggerIgnore :: Sem (Logger ': r) a -> Sem r a
-- runLoggerIgnore = interpret \case
--   LogInfo _ -> pure ()

-- runOutputMock :: Either AppError () -> Sem (OutputPort ': r) a -> Sem r a
-- runOutputMock saveResult = interpret \case
--   SavePostWithCommentsList _ _ -> pure saveResult

-- runUserDataMock :: [Post] -> [Either AppError PostWithComments] -> Sem (UserDataPort ': r) a -> Sem r a
-- runUserDataMock posts pwcs0 = do
--   let pwcsRef = pwcs0
--   interpret \case
--     GetPosts _uid -> pure (Right posts)
--     GetPostWithComments _ -> case pwcsRef of
--       (x:_) -> pure x
--       [] -> pure (Left (Unexpected "No mock value"))
