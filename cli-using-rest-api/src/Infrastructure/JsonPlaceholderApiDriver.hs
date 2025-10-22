{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
module Infrastructure.JsonPlaceholderApiDriver where

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
  , lbsResponse, responseBody, (/:), (/~), https, Url, Scheme(Https)
  )
import Text.URI
  ( mkURI, uriScheme, uriAuthority, uriPath
  , Authority(..), authHost, unRText
  )
import Data.List.NonEmpty (toList)
import Control.Monad (when)
import Data.Bifunctor (first)

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
fetchPosts env uid = liftIO $ withBaseUrl env $ \baseUrl -> do
  let url = baseUrl /: "users" /~ uid /: "posts"
  requestList url

fetchComments :: MonadIO m => Environment -> String -> m (Either AppError [CommentJson])
fetchComments env uid = liftIO $ withBaseUrl env $ \baseUrl -> do
  let url = baseUrl /: "users" /~ uid /: "comments"
  requestList url

withBaseUrl :: Environment -> (Url 'Https -> IO (Either AppError [a])) -> IO (Either AppError [a])
withBaseUrl env action =
  either (pure . Left) action (buildBaseUrl (Text.pack env.apiBaseUrl))

requestList :: FromJSON a => Url 'Https -> IO (Either AppError [a])
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

buildBaseUrl :: Text -> Either AppError (Url 'Https)
buildBaseUrl raw = do
  u <- first (const . Unexpected $ "Invalid base URL format: " <> raw) (mkURI raw)
  schemeTxt <- maybe (Left . Unexpected $ "URL must include scheme") (Right . unRText) (uriScheme u)
  auth <- first (const . Unexpected $ "URL must include host/authority") (uriAuthority u)
  when (schemeTxt /= "https") $ Left . Unexpected $ "Unsupported scheme: " <> schemeTxt
  let hostTxt = unRText . authHost $ auth
      base    = https hostTxt
      segs    = maybe [] (\(_, neSegs) -> unRText <$> toList neSegs) (uriPath u)
  pure $ foldl (/:) base segs