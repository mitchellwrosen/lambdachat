module LambdaChat.Effect.Log where

import Control.Effect
import Control.Effect.Carrier
import Control.Effect.Sum

import qualified Control.Monad.Trans.Reader as Transformers
import qualified Data.Text.IO               as Text


--------------------------------------------------------------------------------
-- Log effect
--------------------------------------------------------------------------------

data Log :: Type -> (Type -> Type) -> Type -> Type where
  Log ::
       a
    -> k
    -> Log a m k
  deriving stock (Functor)

instance Effect (Log a) where
  handle ::
       Functor f
    => f ()
    -> (forall x. f (m x) -> n (f x))
    -> Log a m (m b)
    -> Log a n (n (f b))
  handle state handler (Log x k) =
    Log x (handler (k <$ state))

instance HFunctor (Log a) where
  hmap :: (forall x. m x -> n x) -> Log a m k -> Log a n k
  hmap _ (Log x k) =
    Log x k

log ::
     ( Carrier sig m
     , Member (Log a) sig
     )
  => a
  -> m ()
log x =
  send (Log x (pure ()))

-- contralog ::
--      ( Carrier sig m
--      , Member (Log a) sig
--      )
--   => (a -> b)
--   -> ()
--   -> m x
-- contralog f = undefined

--------------------------------------------------------------------------------
-- Log effect carriers
--------------------------------------------------------------------------------

newtype ContramapLogC s t m a
  = ContramapLogC ((s -> t) -> m a)
  deriving stock (Functor)
  deriving (Applicative, Monad, MonadIO)
       via (Transformers.ReaderT (s -> t) m)

runContramapLog :: (s -> t) -> ContramapLogC s t m a -> m a
runContramapLog f (ContramapLogC m) =
  m f

instance
     ( Carrier sig m
     , Member (Log t) sig
     )
  => Carrier (Log s :+: sig) (ContramapLogC s t m) where

  eff ::
       (Log s :+: sig) (ContramapLogC s t m) (ContramapLogC s t m a)
    -> ContramapLogC s t m a
  eff = \case
    L (Log s k) ->
      ContramapLogC $ \f -> do
        log (f s)
        runContramapLog f k

    R other ->
      ContramapLogC $ \f ->
        eff (handlePure (runContramapLog f) other)


newtype LogStdoutC m a
  = LogStdoutC (m a)
  deriving newtype (Applicative, Functor, Monad, MonadIO)

runLogStdout :: LogStdoutC m a -> m a
runLogStdout (LogStdoutC m) =
  m

instance
     ( Carrier sig m
     , MonadIO m
     )
  => Carrier (Log Text :+: sig) (LogStdoutC m) where

  eff :: (Log Text :+: sig) (LogStdoutC m) (LogStdoutC m a) -> LogStdoutC m a
  eff = \case
    L (Log x k) ->
      LogStdoutC $ do
        liftIO (Text.putStrLn x)
        runLogStdout k

    R other ->
      LogStdoutC (eff (handlePure runLogStdout other))
