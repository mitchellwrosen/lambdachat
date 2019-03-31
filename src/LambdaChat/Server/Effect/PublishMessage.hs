module LambdaChat.Server.Effect.PublishMessage
  ( PublishMessage
  , publishMessage
    -- * Carriers
  , ZMQSenderC
  , runZMQSender
  ) where

import LambdaChat.Effect.FirstOrder

import Control.Effect
import Control.Effect.Carrier
import Control.Effect.Sum

import qualified Control.Monad.Trans.Reader as Transformers
import qualified System.ZMQ4                as ZMQ

data PublishMessage :: (Type -> Type) -> Type -> Type where
  PublishMessage ::
       ByteString
    -> k
    -> PublishMessage m k

  deriving stock (Functor)
  deriving (HFunctor, Effect)
       via (FirstOrderEffect PublishMessage)

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

