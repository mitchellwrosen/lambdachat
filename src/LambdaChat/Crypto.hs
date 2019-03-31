-- | Simple public-key crpytography wrappers.
--
-- The gist of it:
--
--   Ciphertext = Encrypt (MyPrivateKey,   YourPublicKey, Plaintext)
--   Plaintext  = Decrypt (YourPrivateKey, MyPublicKey,   Ciphertext)
--
-- such that Ciphertext does not disclose any information about the message
-- (Plaintext) nor the recipient's identity (YourPublicKey) to an eavesdropper.
--
-- This is accomplished by generating an ephemeral key pair to encrypt and
-- sign the sender's true identity, which is used to encrypt and sign the
-- message.
module LambdaChat.Crypto
  ( -- * Private key
    PrivateKey
  , encodeBase64PrivateKey
  , decodePrivateKey
  , decodeBase64PrivateKey
  , generatePrivateKey
    -- * Public key
  , PublicKey
  , publicKeyToByteString
  , encodeBase64PublicKey
  , decodePublicKey
  , decodeBase64PublicKey
  , derivePublicKey
    -- * Encrypt/decrypt
  , encrypt
  , decrypt
  ) where

import Control.Monad  (guard)
import Crypto.Error
import Crypto.Random
import Data.ByteArray (Bytes, ScrubbedBytes)
import Data.Ord       (comparing)

import qualified Crypto.Cipher.ChaChaPoly1305 as ChaCha20Poly1305
import qualified Crypto.Hash                  as Hash
import qualified Crypto.KDF.HKDF              as HKDF
import qualified Crypto.MAC.Poly1305          as Poly1305
import qualified Crypto.PubKey.Curve25519     as Curve25519
import qualified Data.ByteArray               as ByteArray
import qualified Data.ByteArray.Encoding      as ByteArray.Encoding
import qualified Data.ByteString              as ByteString
import qualified Data.Text                    as Text
import qualified Data.Text.Encoding           as Text


newtype PrivateKey
  = PrivateKey Curve25519.SecretKey

encodeBase64PrivateKey :: PrivateKey -> Text
encodeBase64PrivateKey (PrivateKey privateKey) =
  Text.decodeUtf8
    (ByteArray.Encoding.convertToBase ByteArray.Encoding.Base64
      (ByteArray.convert privateKey :: ByteString))

decodePrivateKey :: ByteString -> Maybe PrivateKey
decodePrivateKey bytes = do
  CryptoPassed privateKey <-
    Just (Curve25519.secretKey bytes)

  pure (PrivateKey privateKey)

-- | Decode a base64-encoded private key.
decodeBase64PrivateKey :: Text -> Maybe PrivateKey
decodeBase64PrivateKey =
  unBase64 >=> decodePrivateKey


newtype PublicKey
  = PublicKey { unPublicKey :: Curve25519.PublicKey }
  deriving newtype (Eq)

instance Ord PublicKey where
  compare =
    comparing
      (ByteArray.convert @_ @Bytes . unPublicKey)

instance Show PublicKey where
  show =
    Text.unpack . encodeBase64PublicKey

publicKeyToByteString :: PublicKey -> ByteString
publicKeyToByteString (PublicKey publicKey) =
  ByteArray.convert publicKey

encodeBase64PublicKey :: PublicKey -> Text
encodeBase64PublicKey (PublicKey publicKey) =
  Text.decodeUtf8
    (ByteArray.Encoding.convertToBase ByteArray.Encoding.Base64
      (ByteArray.convert publicKey :: ByteString))

decodePublicKey :: ByteString -> Maybe PublicKey
decodePublicKey bytes = do
  CryptoPassed publicKey <-
    Just (Curve25519.publicKey bytes)

  pure (PublicKey publicKey)

-- | Decode a base64-encoded public key.
decodeBase64PublicKey :: Text -> Maybe PublicKey
decodeBase64PublicKey =
  unBase64 >=> decodePublicKey

unBase64 :: Text -> Maybe ByteString
unBase64 =
  either (const Nothing) Just
    . ByteArray.Encoding.convertFromBase ByteArray.Encoding.Base64
    . Text.encodeUtf8

generatePrivateKey :: MonadRandom m => m PrivateKey
generatePrivateKey =
  PrivateKey <$> Curve25519.generateSecretKey

derivePublicKey :: PrivateKey -> PublicKey
derivePublicKey (PrivateKey privateKey) =
  PublicKey (Curve25519.toPublic privateKey)

-- | Encrypt and sign an arbitrary payload.
encrypt ::
     MonadRandom m
  => PrivateKey
     -- ^ Sender's private key
  -> PublicKey
     -- ^ Recipient's public key
  -> ByteString
     -- ^ Plaintext
  -> m ByteString
encrypt (PrivateKey privateKey) (PublicKey publicKey) plaintext = do
  nonceBytes :: ByteArray.Bytes <-
    getRandomBytes 12

  let
    nonce :: ChaCha20Poly1305.Nonce
    nonce =
      case ChaCha20Poly1305.nonce12 nonceBytes of
        CryptoFailed err ->
          error (show err)

        CryptoPassed nonce_ ->
          nonce_

  let
    chachaState0 :: ChaCha20Poly1305.State
    chachaState0 =
      initChacha privateKey publicKey nonce

  let
    ciphertext :: ByteString
    chachaState1 :: ChaCha20Poly1305.State
    (ciphertext, chachaState1) =
      ChaCha20Poly1305.encrypt plaintext chachaState0

  let
    polyAuth :: Poly1305.Auth
    polyAuth =
      ChaCha20Poly1305.finalize chachaState1

  pure (ByteArray.convert nonce <> ByteArray.convert polyAuth <> ciphertext)

-- | Decrypt and authenticate an arbitrary payload.
decrypt ::
     PrivateKey
     -- ^ Recipient's private key
  -> PublicKey
     -- ^ Sender's public key
  -> ByteString
     -- ^ Ciphertext
  -> Maybe ByteString
decrypt (PrivateKey privateKey) (PublicKey publicKey) payload0 = do
  let
    (nonceBytes, payload1) =
      ByteString.splitAt 12 payload0

  let
    (expectedPolyAuthBytes, ciphertext) =
      ByteString.splitAt 16 payload1

  CryptoPassed (nonce :: ChaCha20Poly1305.Nonce) <-
    Just (ChaCha20Poly1305.nonce12 nonceBytes)

  CryptoPassed (expectedPolyAuth :: Poly1305.Auth) <-
    Just (Poly1305.authTag expectedPolyAuthBytes)

  let
    chachaState0 :: ChaCha20Poly1305.State
    chachaState0 =
      initChacha privateKey publicKey nonce

  let
    plaintext :: ByteString
    chachaState1 :: ChaCha20Poly1305.State
    (plaintext, chachaState1) =
      ChaCha20Poly1305.decrypt ciphertext chachaState0

  let
    actualPolyAuth :: Poly1305.Auth
    actualPolyAuth =
      ChaCha20Poly1305.finalize chachaState1

  guard (actualPolyAuth == expectedPolyAuth)

  pure plaintext

initChacha ::
     Curve25519.SecretKey
  -> Curve25519.PublicKey
  -> ChaCha20Poly1305.Nonce
  -> ChaCha20Poly1305.State
initChacha privateKey publicKey nonce =
  case ChaCha20Poly1305.initialize (deriveKey privateKey publicKey) nonce of
    CryptoFailed err ->
      error (show err)

    CryptoPassed state ->
      state

deriveKey :: Curve25519.SecretKey -> Curve25519.PublicKey -> ScrubbedBytes
deriveKey privateKey publicKey =
  ByteArray.convert
    (HKDF.extractSkip
      (Curve25519.dh publicKey privateKey) :: HKDF.PRK Hash.Blake2sp_256)
