{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Infrastructure.JsonPlaceholderApiDriver (
  PostJson(..)
 , CommentJson(..)
 , fetchPosts
 , fetchComments
 , requestList
 ) where

import Data.Aeson (FromJSON, eitherDecode)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.ByteString.Lazy as LBS
import GHC.Generics (Generic)
import Environment
import Control.Monad.IO.Class (MonadIO (..))
import Control.Exception (try, SomeException)
import Application.Error (AppError(..))
import Network.HTTP.Req
  ( runReq, defaultHttpConfig, req, GET(..), NoReqBody(..)
  , lbsResponse, responseBody, (/:), (/~), Url
  )
import BaseUrl (pattern BaseUrl)

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
fetchPosts (Environment (BaseUrl base)) uid = liftIO $ do
  requestList $ base /: "users" /~ uid /: "posts"

fetchComments :: MonadIO m => Environment -> String -> m (Either AppError [CommentJson])
fetchComments (Environment (BaseUrl base)) uid = liftIO $ do
  requestList $ base /: "users" /~ uid /: "comments"

requestList :: FromJSON a => Url scheme -> IO (Either AppError [a])
requestList url = do
  netResult <- try $ runReq defaultHttpConfig $ do
    resp <- req GET url NoReqBody lbsResponse mempty
    pure $ decodeList (responseBody resp)
  pure $ either (Left . NetworkError . packSome) id netResult

decodeList :: FromJSON a => LBS.ByteString -> Either AppError [a]
decodeList bs =
  case eitherDecode bs of
    Left err -> Left (DecodeError (toText err))
    Right xs -> Right xs

toText :: String -> Text
toText = Text.pack

packSome :: SomeException -> Text
packSome = toText . show


