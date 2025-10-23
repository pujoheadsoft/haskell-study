{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DeriveGeneric #-}
module Environment where

import Optics.TH (makeFieldLabels)
import BaseUrl (BaseUrl)
import GHC.Generics (Generic)
import System.Envy (FromEnv (..), env)

data Environment = Environment {
  apiBaseUrl :: String
} deriving (Show)

makeFieldLabels ''Environment

data Setting = Setting {
  settingApiBaseUrl :: BaseUrl
} deriving (Show, Generic)

-- instance FromEnv Setting where
--   fromEnv = do
--     mBaseUrl <- envMaybe "API_BASE_URL"
--     case mBaseUrl of
--       Just baseUrl -> return $ Setting baseUrl
--       Nothing      -> fail "API_BASE_URL environment variable not set"