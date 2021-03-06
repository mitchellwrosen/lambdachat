module LambdaChat.Server.Effect.KeyValueStore
  ( KeyValueStore
  , kvStoreGet
  , kvStorePut
    -- * Carriers
  , MapKeyValueStoreC
  , runMapKeyValueStore
  ) where

import LambdaChat.Effect.FirstOrder

import Control.Effect
import Control.Effect.Carrier
import Control.Effect.State   (StateC(..))
import Control.Effect.Sum

import qualified Data.Map as Map


data KeyValueStore (k :: Type) (v :: Type) (m :: Type -> Type) (a :: Type) where
  Get ::
       k
    -> (Maybe v -> a)
    -> KeyValueStore k v m a

  Put ::
       k
    -> v
    -> a
    -> KeyValueStore k v m a

  deriving stock (Functor)
  deriving (HFunctor, Effect)
       via (FirstOrderEffect (KeyValueStore k v))

kvStoreGet ::
     ( Carrier sig m
     , Member (KeyValueStore k v) sig
     )
  => k
  -> m (Maybe v)
kvStoreGet key =
  send (Get key pure)

kvStorePut ::
     ( Carrier sig m
     , Member (KeyValueStore k v) sig
     )
  => k
  -> v
  -> m ()
kvStorePut key value =
  send (Put key value (pure ()))


-- | A key-value store backed by a Map.
newtype MapKeyValueStoreC k v m a
  = MapKeyValueStoreC (Map k v -> m (Map k v, a))
  deriving (Applicative, Functor, Monad)
       via (StateC (Map k v) m)

runMapKeyValueStore :: Map k v -> MapKeyValueStoreC k v m a -> m (Map k v, a)
runMapKeyValueStore store (MapKeyValueStoreC f) =
  f store

instance
     ( Carrier sig m
     , Effect sig
     , Ord k
     )
  => Carrier (KeyValueStore k v :+: sig) (MapKeyValueStoreC k v m) where

  eff ::
       (KeyValueStore k v :+: sig) (MapKeyValueStoreC k v m) (MapKeyValueStoreC k v m a)
    -> MapKeyValueStoreC k v m a
  eff = \case
    L (Get key next) ->
      MapKeyValueStoreC $ \store ->
        runMapKeyValueStore store (next (Map.lookup key store))

    L (Put key value next) ->
      MapKeyValueStoreC $ \store ->
        let
          !store' =
            Map.insert key value store
        in
          runMapKeyValueStore store' next

    R other ->
      MapKeyValueStoreC $ \store ->
        eff (handle (store, ()) (uncurry runMapKeyValueStore) other)
