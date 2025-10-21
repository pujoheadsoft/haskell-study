{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Environment where

import Optics.TH (makeFieldLabels)

data Environment = Environment {
  apiBaseUrl :: String
} deriving (Show)

makeFieldLabels ''Environment
