module LambdaChat.Server.Effect.GenerateUUID
  ( GenerateUUID
  , generateUUID
    -- * Carriers
  , GenerateUUIDC
  , runGenerateUUID
  ) where

import LambdaChat.Effect.FirstOrder

import Control.Effect
import Control.Effect.Carrier
import Control.Effect.Sum
import Data.UUID (UUID)

import qualified Data.UUID.V4 as UUID


data GenerateUUID (uuid :: Type) (m :: Type -> Type) (k :: Type) where
  GenerateUUID ::
       (uuid -> a)
    -> GenerateUUID uuid m a

  deriving stock (Functor)
  deriving (HFunctor, Effect)
       via (FirstOrderEffect (GenerateUUID uuid))

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

runGenerateUUID :: GenerateUUIDC m a -> m a
runGenerateUUID (GenerateUUIDC m) =
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
        liftIO UUID.nextRandom >>= runGenerateUUID . k

    R other ->
      GenerateUUIDC (eff (handlePure runGenerateUUID other))
