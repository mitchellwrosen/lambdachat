module LambdaChat.Server.Chat where

import Data.Time (UTCTime)
import Data.UUID (UUID)


data Chat
  = Chat
  { uuid :: UUID
  , timestamp :: UTCTime
  , message :: ByteString
  }
