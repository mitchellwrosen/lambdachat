module LambdaChat.Server.Chat where

import LambdaChat.Server.LogMessage

import Data.Time (UTCTime)
import Data.UUID (UUID)

import qualified Data.UUID as UUID


data Chat
  = Chat
  { uuid :: UUID
  , timestamp :: UTCTime
  , message :: ByteString
  } deriving stock (Generic, Show)

chatToLogMessage :: Chat -> LogMessage
chatToLogMessage chat =
  debugLogMessage ("chat " <> UUID.toText (uuid chat))
