module LambdaChat.Server.Effect.ReceiveMessage
  ( ReceiveMessage
  , receiveMessage
    -- * Carriers
  , ZMQReceiverC
  , runZMQReceiver
  ) where

import LambdaChat.Effect.FirstOrder

import Control.Effect
import Control.Effect.Carrier
import Control.Effect.Sum

import qualified Control.Monad.Trans.Reader as Transformers
import qualified System.ZMQ4                as ZMQ


data ReceiveMessage :: (Type -> Type) -> Type -> Type where
  ReceiveMessage ::
       (ByteString -> k)
    -> ReceiveMessage m k

  deriving stock (Functor)
  deriving (HFunctor, Effect)
       via (FirstOrderEffect ReceiveMessage)

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
