{-# LANGUAGE GADTs #-}
module BaseUrl where
import Data.Text (Text)
import Application.Error (AppError (..))
import Network.HTTP.Req (Url, Scheme, (/:), https, http)
import Text.URI (unRText, Authority (authHost), mkURI, URI (..))
import Data.Foldable (toList)
import Data.Bifunctor (first)

buildBaseUrl :: Text -> Either AppError BaseUrl
buildBaseUrl raw = do
  u <- first (const . Unexpected $ "Invalid base URL format: " <> raw) (mkURI raw)
  schemeTxt <- maybe (Left . Unexpected $ "URL must include scheme") (Right . unRText) (uriScheme u)
  auth <- first (const . Unexpected $ "URL must include host/authority") (uriAuthority u)
  let hostTxt = unRText (authHost auth)
      segs    = maybe [] (\(_, neSegs) -> unRText <$> toList neSegs) (uriPath u)
  case schemeTxt of
    "https" -> pure . BaseUrl $ foldl (/:) (https hostTxt) segs
    "http"  -> pure . BaseUrl $ foldl (/:) (http hostTxt) segs
    other    -> Left . Unexpected $ "Unsupported scheme: " <> other

data BaseUrl where
  BaseUrl :: Url (s :: Scheme) -> BaseUrl

instance Show BaseUrl where
  show (BaseUrl _) = "BaseUrl <Url>"