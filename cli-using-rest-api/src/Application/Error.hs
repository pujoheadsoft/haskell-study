module Application.Error (
  AppError(..),
) where

import Data.Text (Text)
import Control.Exception (Exception)

data AppError
  = NetworkError Text
  | DecodeError Text
  | NotFound Text
  | Unexpected Text
  deriving (Show, Eq)

instance Exception AppError
