{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Environment (Environment(..), loadEnvironment) where

import BaseUrl (BaseUrl, parseBaseUrl)
import Optics.TH (makeFieldLabels)
import System.Envy (FromEnv (..), decodeEnv, Parser, envMaybe)
import Control.Exception (throwIO)
import Application.Error (AppError (..))
import Data.Text (Text, unpack, pack)

data Environment = Environment {
  apiBaseUrl :: BaseUrl
} deriving (Show)
makeFieldLabels ''Environment

instance FromEnv Environment where
  fromEnv _ = Environment
    <$> envCustomValidate "API_BASE_URL" "https://jsonplaceholder.typicode.com" parseBaseUrl

loadEnvironment :: IO Environment
loadEnvironment = do
  r <- decodeEnv
  case r of
    Left msg -> throwIO (Unexpected (pack msg))
    Right env -> pure env

envCustomValidate :: Text -> Text -> (Text -> Either AppError a) -> Parser a
envCustomValidate key def parser = do
  let k = unpack key
  mval <- envMaybe k
  let raw = maybe def pack mval
  case parser raw of
    Right v -> pure v
    Left e -> fail (k <> " validation failed: " <> show e)
