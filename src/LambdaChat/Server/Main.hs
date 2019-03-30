module LambdaChat.Server.Main where

import LambdaChat.Effect.Log
import LambdaChat.Server.Effect.PublishMessage
import LambdaChat.Server.Effect.ReceiveMessage

import Control.Effect

import qualified System.ZMQ4 as ZMQ


main :: IO ()
main = do
  ZMQ.withContext $ \context ->
    ZMQ.withSocket context ZMQ.Pull $ \pullSocket ->
      ZMQ.withSocket context ZMQ.Pub $ \pubSocket -> do
        ZMQ.bind pullSocket "ipc://lambdachat-pull.sock"
        ZMQ.bind pubSocket "ipc://lambdachat-pub.sock"

        doMain
          & runContramapLog renderLogMessage
          & runLogStdout
          & runZMQReceiver pullSocket
          & runZMQSender pubSocket
          & runM

newtype LogMessage
  = LogMessage Text

renderLogMessage :: LogMessage -> Text
renderLogMessage (LogMessage msg) =
  msg

doMain ::
     ( Carrier sig m
     , Member (Log LogMessage) sig
     , Member PublishMessage sig
     , Member ReceiveMessage sig
     )
  => m ()
doMain = do
  message <- receiveMessage
  log (LogMessage "Message received")
  publishMessage message
  log (LogMessage "Message published")
  doMain
