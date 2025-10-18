module Domain.Post where

import Data.Text (Text)

type UserId = String
type PostId = String
type CommentId = String

data Post = Post
  { id    :: PostId
  , title :: Text
  , body  :: Text
  } deriving (Show, Eq)

data Comment = Comment
  { id       :: CommentId
  , name     :: Text
  , email    :: Text
  , body     :: Text
  } deriving (Show, Eq)

data PostWithComments = PostWithComments
  { post     :: Post
  , comments :: [Comment]
  } deriving (Show, Eq)
