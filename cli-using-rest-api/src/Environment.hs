{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Environment where

import Control.Lens (makeFieldsId)

data Environment = Environment {
  apiBaseUrl :: String
} deriving (Show)

makeFieldsId ''Environment
