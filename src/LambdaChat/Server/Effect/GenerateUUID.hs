module LambdaChat.Server.Effect.GenerateUUID
  ( GenerateUUID
  , generateUUID
    -- * Carriers
  , GenerateUUIDC
  , runGenerateUUIDC
  ) where

import Control.Effect
import Control.Effect.Carrier
import Control.Effect.Sum
import Data.UUID (UUID)

import qualified Data.UUID.V4 as UUID


data GenerateUUID :: Type -> (Type -> Type) -> Type -> Type where
  GenerateUUID ::
       (uuid -> a)
    -> GenerateUUID uuid m a

  deriving stock (Functor)

instance Effect (GenerateUUID uuid) where
  handle ::
       Functor f
    => f ()
    -> (forall x. f (m x) -> n (f x))
    -> GenerateUUID uuid m (m a)
    -> GenerateUUID uuid n (n (f a))
  handle state handler (GenerateUUID k) =
    GenerateUUID (handler . (<$ state) . k)

instance HFunctor (GenerateUUID uuid) where
  hmap ::
       (forall x. m x -> n x)
    -> GenerateUUID uuid m a
    -> GenerateUUID uuid n a
  hmap _ =
    coerce

generateUUID ::
     forall uuid m sig.
     ( Carrier sig m
     , Member (GenerateUUID uuid) sig
     )
  => m uuid
generateUUID =
  send (GenerateUUID pure)


--------------------------------------------------------------------------------
-- Carriers
--------------------------------------------------------------------------------

newtype GenerateUUIDC m a
  = GenerateUUIDC (m a)
  deriving newtype (Applicative, Functor, Monad, MonadIO)

runGenerateUUIDC :: GenerateUUIDC m a -> m a
runGenerateUUIDC (GenerateUUIDC m) =
  m

instance
     ( Carrier sig m
     , MonadIO m
     )
  => Carrier (GenerateUUID UUID :+: sig) (GenerateUUIDC m) where

  eff ::
       (GenerateUUID UUID :+: sig) (GenerateUUIDC m) (GenerateUUIDC m a)
    -> GenerateUUIDC m a
  eff = \case
    L (GenerateUUID k) ->
      GenerateUUIDC $ do
        liftIO UUID.nextRandom >>= runGenerateUUIDC . k

    R other ->
      GenerateUUIDC (eff (handlePure runGenerateUUIDC other))
