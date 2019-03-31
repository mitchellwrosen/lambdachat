module LambdaChat.Client.Config
  ( Config(..)
  , parseConfig
  , InvalidPublicKey(..)
  , InvalidPrivateKey(..)
  ) where

import LambdaChat.Crypto

import Control.Exception.Safe (toException, tryAny)

import qualified Dhall

data UnparsedConfig
  = UnparsedConfig
  { identity :: Maybe Text
  , peers :: [UnparsedPeer]
  } deriving stock (Generic)
    deriving anyclass (Dhall.Interpret)

data UnparsedPeer
  = UnparsedPeer
  { name :: Text
  , identity :: Text
  } deriving stock (Generic)
    deriving anyclass (Dhall.Interpret)

data Config
  = Config
  { identity :: Maybe PrivateKey
  , peers :: [(Text, PublicKey)]
  }

newtype InvalidPublicKey
  = InvalidPublicKey Text
  deriving stock (Show)
  deriving anyclass (Exception)

newtype InvalidPrivateKey
  = InvalidPrivateKey Text
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Parse the client config.
--
-- /Throws/: This function will never throw an exception.
parseConfig :: FilePath -> IO (Either SomeException Config)
parseConfig configFile =
  tryAny (Dhall.inputFile Dhall.auto configFile) >>= \case
    Left ex ->
      pure (Left ex)
    Right unparsedConfig ->
      pure (parseConfig_ unparsedConfig)

parseConfig_ :: UnparsedConfig -> Either SomeException Config
parseConfig_ UnparsedConfig { identity, peers } = do
  identity <- parsePrivateKey identity
  peers <- traverse parsePeer peers
  Right Config { identity, peers }

parsePeer :: UnparsedPeer -> Either SomeException (Text, PublicKey)
parsePeer UnparsedPeer { name, identity } =
  (name,) <$> parsePublicKey identity

parsePublicKey :: Text -> Either SomeException PublicKey
parsePublicKey bytes =
  case decodeBase64PublicKey bytes of
    Nothing  -> Left (toException (InvalidPublicKey bytes))
    Just key -> Right key

parsePrivateKey :: Maybe Text -> Either SomeException (Maybe PrivateKey)
parsePrivateKey = \case
  Nothing ->
    Right Nothing

  Just bytes ->
    case decodeBase64PrivateKey bytes of
      Nothing  -> Left (toException (InvalidPrivateKey bytes))
      Just key -> Right (Just key)
