{-# LANGUAGE NoImplicitPrelude #-}

module Prelude
  ( module X
  ) where

import Control.Exception.Safe as X (Exception, SomeException, throwIO)
import Control.Lens           as X ((^.))
import Control.Monad          as X (forever, (>=>))
import Control.Monad.IO.Class as X (MonadIO(..))
import Data.ByteString        as X (ByteString)
import Data.Coerce            as X (Coercible, coerce)
import Data.Foldable          as X (fold)
import Data.Function          as X ((&))
import Data.Kind              as X (Type)
import Data.Map               as X (Map)
import Data.Text              as X (Text)
import GHC.Generics           as X (Generic)
import PreludeFromBase        as X hiding (log)

import Data.Generics.Labels ()
