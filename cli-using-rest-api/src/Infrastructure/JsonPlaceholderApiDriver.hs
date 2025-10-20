{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
module Infrastructure.JsonPlaceholderApiDriver where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Environment
import Network.Wreq (get, responseBody)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Lens ((^.))
import Control.Concurrent (threadDelay)
import Control.Exception (try, SomeException)
import Application.Error (AppError(..))

data PostJson = PostJson
  { userId :: Int
  , id     :: Int
  , title  :: Text
  , body   :: Text
  } deriving (Show, Generic)
instance FromJSON PostJson

data CommentJson = CommentJson
  { postId :: Int
  , id     :: Int
  , name   :: Text
  , email  :: Text
  , body   :: Text
  } deriving (Show, Generic)
instance FromJSON CommentJson

fetchPosts :: MonadIO m => Environment -> String -> m (Either AppError [PostJson])
fetchPosts env userId = do
  let url = env.apiBaseUrl <> "/users/" <> userId <> "/posts"
  eResp <- liftIO $ try (get url)
  case eResp of
    Left ex -> pure $ Left (NetworkError (packSome ex))
    Right response ->
      case eitherDecode (response ^. responseBody) of
        Left err -> pure $ Left (DecodeError (toText err))
        Right posts -> pure $ Right posts

fetchComments :: MonadIO m => Environment -> String -> m (Either AppError [CommentJson])
fetchComments env userId = do
  let url = env.apiBaseUrl <> "/users/" <> userId <> "/comments"
  liftIO $ putStrLn $ "Fetching comments from: " <> url
  eResp <- liftIO $ try (threadDelay 1000000 >> get url)
  case eResp of
    Left ex -> pure $ Left (NetworkError (packSome ex))
    Right response -> do
      liftIO $ putStrLn "Fetched comments, decoding..."
      case eitherDecode (response ^. responseBody) of
        Left err -> pure $ Left (DecodeError (toText err))
        Right comments -> pure $ Right comments

-- helpers
toText :: String -> Text
toText = Text.pack
packSome :: SomeException -> Text
packSome = toText . show
