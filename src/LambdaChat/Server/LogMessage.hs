module LambdaChat.Server.LogMessage
  ( LogMessage
  , debugLogMessage
  , infoLogMessage
  , renderLogMessage
  ) where


data LogMessage
  = LogMessage Level Text

data Level
  = Debug
  | Info

renderLogMessage :: LogMessage -> Text
renderLogMessage (LogMessage level message) =
  case level of
    Debug -> "[debug] " <> message
    Info  -> "[info]  " <> message

debugLogMessage :: Text -> LogMessage
debugLogMessage =
  LogMessage Debug

infoLogMessage :: Text -> LogMessage
infoLogMessage =
  LogMessage Info
