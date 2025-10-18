{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
module Infrastructure.JsonPlaceholderApiDriver where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import Environment
import Network.Wreq (get, responseBody)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Lens ((^.))
import Control.Concurrent (threadDelay)

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

fetchPosts :: MonadIO m => Environment -> String -> m [PostJson]
fetchPosts env userId = do
  let 
    url = env.apiBaseUrl <> "/users/" <> userId <> "/posts"
  response <- liftIO $ get url
  case eitherDecode (response ^. responseBody) of
    Left err -> error $ "Failed to decode posts: " <> err
    Right posts -> pure posts

fetchComments :: MonadIO m => Environment -> String -> m [CommentJson]
fetchComments env userId = do
  let 
    url = env.apiBaseUrl <> "/users/" <> userId <> "/comments"
  liftIO $ putStrLn $ "Fetching comments from: " <> url
  response <- liftIO $ threadDelay 1000000 >> get url
  liftIO $ putStrLn $ "Fetched comments, decoding..."
  case eitherDecode (response ^. responseBody) of
    Left err -> error $ "Failed to decode comments: " <> err
    Right comments -> pure comments
