module Domain.Error where

newtype Error = Error String deriving (Show, Eq)