module LambdaChat.Server.Effect.ChatStorage
  ( ChatStorage
  , storeChat
  , searchChat
    -- * Carriers
  , MapChatStorageC
  , runMapChatStorage
  ) where

import LambdaChat.Effect.FirstOrder
import LambdaChat.Server.Chat

import Control.Effect
import Control.Effect.Carrier
import Control.Effect.State
import Control.Effect.Sum
import Data.Time              (UTCTime)

import qualified Data.Map as Map


data ChatStorage :: (Type -> Type) -> Type -> Type where
  Store ::
       Chat
    -> k
    -> ChatStorage m k

  Search ::
       UTCTime
    -> ([Chat] -> k)
    -> ChatStorage m k

  deriving stock (Functor)

  deriving (HFunctor, Effect)
       via (FirstOrderEffect ChatStorage)

storeChat ::
     ( Carrier sig m
     , Member ChatStorage sig
     )
  => Chat
  -> m ()
storeChat chat =
  send (Store chat (pure ()))

searchChat ::
     ( Carrier sig m
     , Member ChatStorage sig
     )
  => UTCTime
  -> m [Chat]
searchChat timestamp =
  send (Search timestamp pure)


--------------------------------------------------------------------------------
-- Carriers
--------------------------------------------------------------------------------

newtype MapChatStorageC m a
  = MapChatStorageC (StateC (Map UTCTime [Chat]) m a)
  deriving newtype (Applicative, Functor, Monad, MonadIO)

runMapChatStorage ::
     forall a m.
     Map UTCTime [Chat]
  -> MapChatStorageC m a
  -> m (Map UTCTime [Chat], a)
runMapChatStorage =
  coerce
    (runState ::
         Map UTCTime [Chat]
      -> StateC (Map UTCTime [Chat]) m a
      -> m (Map UTCTime [Chat], a))

storeMapChat ::
     ( Carrier sig m
     , Effect sig
     )
  => Chat
  -> MapChatStorageC m ()
storeMapChat chat =
  MapChatStorageC (modify insert)

  where
    insert :: Map UTCTime [Chat] -> Map UTCTime [Chat]
    insert =
      Map.insertWith (++) (chat ^. #timestamp) [chat]

searchMapChat ::
     ( Carrier sig m
     , Effect sig
     )
  => UTCTime
  -> MapChatStorageC m [Chat]
searchMapChat timestamp =
  MapChatStorageC (gets f)

  where
    f :: Map UTCTime [Chat] -> [Chat]
    f =
      concat . Map.elems . Map.dropWhileAntitone (< timestamp)

instance
     ( Carrier sig m
     , Effect sig
     )
  => Carrier (ChatStorage :+: sig) (MapChatStorageC m) where

  eff ::
       (ChatStorage :+: sig) (MapChatStorageC m) (MapChatStorageC m a)
    -> MapChatStorageC m a
  eff = \case
    L (Store chat next) -> do
      storeMapChat chat
      next

    L (Search timestamp next) ->
      searchMapChat timestamp >>= next

    R other ->
      MapChatStorageC (eff (R (handleCoercible other)))

