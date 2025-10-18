{-# LANGUAGE DeriveGeneric #-}
module Infrastructure.FileDriver where

import Prelude hiding (writeFile)
import Data.Aeson (ToJSON)
import Data.Aeson.Encode.Pretty (encodePretty', defConfig, Config (..), Indent (Tab))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString.Lazy.Char8 (writeFile)
import Data.Text (Text)
import GHC.Generics (Generic)

data PostJson = PostJson
  { title :: Text
  , body  :: Text
  } deriving (Show, Generic)
instance ToJSON PostJson

data CommentJson = CommentJson
  { name     :: Text
  , email    :: Text
  , body     :: Text
  } deriving (Show, Generic)
instance ToJSON CommentJson
  
data PostWithCommentsJson = PostWithCommentsJson
  { post     :: PostJson
  , comments :: [CommentJson]
  } deriving (Show, Generic)
instance ToJSON PostWithCommentsJson

newtype PostWithCommentsListJson = PostWithCommentsListJson
  { postsWithComments :: [PostWithCommentsJson]
  } deriving (Show, Generic)
instance ToJSON PostWithCommentsListJson

saveAsJson 
  :: (ToJSON a, MonadIO m) 
  => FilePath
  -> a
  -> m ()
saveAsJson filePath a = liftIO $ do
  let jsonContent = encodePretty' defConfig { confIndent = Tab } a

  writeFile filePath jsonContent

  putStrLn $ "JSON を書き込みました: " ++ filePath