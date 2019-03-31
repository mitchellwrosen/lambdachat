module LambdaChat.Server.Main where

import LambdaChat.Effect.Log
import LambdaChat.Proto.Message
import LambdaChat.Server.Chat
import LambdaChat.Server.Effect.ChatStorage
import LambdaChat.Server.Effect.GenerateUUID
import LambdaChat.Server.Effect.PublishMessage
import LambdaChat.Server.Effect.ReceiveMessage
import LambdaChat.Server.LogMessage

import qualified Capnp.Gen.Protocol.Message.Pure as LambdaChat.Capnp

import Control.Effect
import Data.Time      (getCurrentTime)
import Data.UUID      (UUID)

import qualified Data.Map    as Map
import qualified System.ZMQ4 as ZMQ


main :: IO ()
main = do
  ZMQ.withContext $ \context ->
    ZMQ.withSocket context ZMQ.Pull $ \pullSocket ->
      ZMQ.withSocket context ZMQ.Pub $ \pubSocket -> do
        ZMQ.bind pullSocket "ipc://lambdachat-pull.sock"
        ZMQ.bind pubSocket "ipc://lambdachat-pub.sock"

        (chats, ()) <-
          doMain
            & runMapChatStorage Map.empty      -- ChatStorage
            & runGenerateUUID                  -- GenerateUUID
            & runContramapLog chatToLogMessage -- Log Chat ==> Log LogMessage
            & runContramapLog renderLogMessage -- Log LogMessage ==> Log Text
            & runLogStdout                     -- Log Text
            & runZMQReceiver pullSocket        -- ReceiveMessage
            & runZMQSender pubSocket           -- PublishMessage
            & runM

        print chats

doMain ::
     ( Carrier sig m
     , Member ChatStorage sig
     , Member (GenerateUUID UUID) sig
     , Member (Log Chat) sig
     , Member PublishMessage sig
     , Member ReceiveMessage sig
     , MonadIO m
     )
  => m ()
doMain =
  forever $ do
    bytes <- receiveMessage

    case decodeProtoMessage bytes of
      Nothing ->
        pure ()

      Just message ->
        case message of
          LambdaChat.Capnp.Message
            { chat =
                LambdaChat.Capnp.ChatMessage
                  { message =
                      message
                  }
            } -> do

            now <- liftIO getCurrentTime
            uuid <- generateUUID @UUID

            let
              chat :: Chat
              chat =
                Chat
                  { uuid = uuid
                  , timestamp = now
                  , message = message
                  }

            log chat
            storeChat chat

            publishMessage message
