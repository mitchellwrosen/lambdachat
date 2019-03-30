module Main where

import LambdaChat.Effect.Log

import Control.Effect
import Control.Effect.Carrier
import Control.Effect.Sum

import qualified Control.Monad.Trans.Reader as Transformers
import qualified System.ZMQ4                as ZMQ


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


--------------------------------------------------------------------------------
-- ReceiveMessage effect
--------------------------------------------------------------------------------

data ReceiveMessage :: (Type -> Type) -> Type -> Type where
  ReceiveMessage ::
       (ByteString -> k)
    -> ReceiveMessage m k
  deriving stock (Functor)

instance Effect ReceiveMessage where
  handle ::
       Functor f
    => f ()
    -> (forall x. f (m x) -> n (f x))
    -> ReceiveMessage m (m a)
    -> ReceiveMessage n (n (f a))
  handle state handler (ReceiveMessage f) =
    ReceiveMessage (handler . (<$ state) . f)

instance HFunctor ReceiveMessage where
  hmap _ (ReceiveMessage f) =
    ReceiveMessage f

receiveMessage ::
     ( Carrier sig m
     , Member ReceiveMessage sig
     )
  => m ByteString
receiveMessage =
  send (ReceiveMessage pure)

newtype ZMQReceiverC t m a
  = ZMQReceiverC { unZMQReceiverC :: ZMQ.Socket t -> m a }
  deriving stock (Functor)
  deriving (Applicative, Monad, MonadIO)
       via (Transformers.ReaderT (ZMQ.Socket t) m)

runZMQReceiver :: ZMQ.Socket t -> ZMQReceiverC t m a -> m a
runZMQReceiver =
  flip unZMQReceiverC

instance
     ( Carrier sig m
     , MonadIO m
     , ZMQ.Receiver t
     )
  => Carrier (ReceiveMessage :+: sig) (ZMQReceiverC t m) where

  eff (L (ReceiveMessage f)) =
    ZMQReceiverC $ \socket ->
      liftIO (ZMQ.receive socket) >>= runZMQReceiver socket . f

  eff (R other) =
    ZMQReceiverC $ \socket ->
      eff (handlePure (runZMQReceiver socket) other)


--------------------------------------------------------------------------------
-- PublishMessage effect
--------------------------------------------------------------------------------

data PublishMessage :: (Type -> Type) -> Type -> Type where
  PublishMessage ::
       ByteString
    -> k
    -> PublishMessage m k
  deriving stock (Functor)

instance Effect PublishMessage where
  handle state handler (PublishMessage message k) =
    PublishMessage message (handler (k <$ state))

instance HFunctor PublishMessage where
  hmap _ (PublishMessage m k) =
    PublishMessage m k

publishMessage ::
     ( Carrier sig m
     , Member PublishMessage sig
     )
  => ByteString
  -> m ()
publishMessage message =
  send (PublishMessage message (pure ()))

newtype ZMQSenderC t m a
  = ZMQSenderC { unZMQSenderC :: ZMQ.Socket t -> m a }
  deriving stock (Functor)
  deriving (Applicative, Monad, MonadIO)
       via (Transformers.ReaderT (ZMQ.Socket t) m)

runZMQSender :: ZMQ.Socket t -> ZMQSenderC t m a -> m a
runZMQSender =
  flip unZMQSenderC

instance
     ( Carrier sig m
     , MonadIO m
     , ZMQ.Sender t
     )
  => Carrier (PublishMessage :+: sig) (ZMQSenderC t m) where

  eff (L (PublishMessage message k)) =
    ZMQSenderC $ \socket -> do
      liftIO (ZMQ.send socket [] message)
      runZMQSender socket k

  eff (R other) =
    ZMQSenderC $ \socket ->
      eff (handlePure (runZMQSender socket) other)
