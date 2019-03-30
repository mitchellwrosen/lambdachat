module LambdaChat.Server.Main where

import LambdaChat.Effect.Log
import LambdaChat.Server.Effect.GenerateUUID
import LambdaChat.Server.Effect.PublishMessage
import LambdaChat.Server.Effect.ReceiveMessage

import Control.Effect
import Data.UUID      (UUID)

import qualified Data.UUID   as UUID
import qualified System.ZMQ4 as ZMQ


main :: IO ()
main = do
  ZMQ.withContext $ \context ->
    ZMQ.withSocket context ZMQ.Pull $ \pullSocket ->
      ZMQ.withSocket context ZMQ.Pub $ \pubSocket -> do
        ZMQ.bind pullSocket "ipc://lambdachat-pull.sock"
        ZMQ.bind pubSocket "ipc://lambdachat-pub.sock"

        doMain
          & runGenerateUUIDC
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
     , Member (GenerateUUID UUID) sig
     , Member (Log LogMessage) sig
     , Member PublishMessage sig
     , Member ReceiveMessage sig
     )
  => m ()
doMain =
  forever $ do
    message <- receiveMessage
    uuid <- generateUUID @UUID
    log (LogMessage ("Received message " <> UUID.toText uuid))
    publishMessage message
