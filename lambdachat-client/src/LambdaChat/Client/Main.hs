module LambdaChat.Client.Main
  ( main
  ) where

import LambdaChat.Client.Config
import LambdaChat.Crypto

import Control.Concurrent (forkIO)
import Crypto.Random      (MonadRandom(..))
import Data.Foldable      (for_)
import System.Environment (getArgs)

import qualified Data.ByteString       as ByteString
import qualified Data.ByteString.Char8 as Latin1
import qualified Data.Text.IO          as Text
import qualified System.ZMQ4           as ZMQ

main :: IO ()
main = do
  [configFile] <-
    getArgs

  Config { identity, peers } <-
    throwLeft (parseConfig configFile)

  identity :: PrivateKey <-
    case identity of
      Nothing -> do
        key <- generatePrivateKey
        Text.putStrLn ("Private key: " <> encodeBase64PrivateKey key)
        Text.putStrLn ("Public key:  " <> encodeBase64PublicKey (derivePublicKey key))
        pure key
      Just identity ->
        pure identity

  ZMQ.withContext $ \context ->
    ZMQ.withSocket context ZMQ.Push $ \pushSocket ->
      ZMQ.withSocket context ZMQ.Sub $ \subSocket -> do
        ZMQ.connect pushSocket "ipc://lambdachat-pull.sock"
        ZMQ.connect subSocket "ipc://lambdachat-pub.sock"
        ZMQ.subscribe subSocket ""

        _ <-
          forkIO . forever $ do
            bytes <- ZMQ.receive subSocket
            case decryptMessage identity bytes of
              Nothing -> pure ()
              Just message -> print message

        forever $ do
          message :: ByteString <-
            Latin1.getLine

          for_ peers $ \peer -> do
            ciphertext :: ByteString <-
              encryptMessage identity peer message

            ZMQ.send pushSocket [] ciphertext

encryptMessage ::
     MonadRandom m
  => PrivateKey
  -> PublicKey
  -> ByteString
  -> m ByteString
encryptMessage privateKey publicKey plaintext = do
  ephemeralPrivateKey :: PrivateKey <-
    generatePrivateKey

  encryptedPublicKey :: ByteString <-
    encrypt
      ephemeralPrivateKey
      publicKey
      (publicKeyToByteString (derivePublicKey privateKey))

  ciphertext :: ByteString <-
    encrypt
      privateKey
      publicKey
      plaintext

  pure
    (ByteString.concat
      [ publicKeyToByteString (derivePublicKey ephemeralPrivateKey)
      , encryptedPublicKey
      , ciphertext
      ])

decryptMessage ::
     PrivateKey
  -> ByteString
  -> Maybe (PublicKey, ByteString)
decryptMessage privateKey payload0 = do
  let
    (ephemeralPublicKeyBytes, payload1) =
      ByteString.splitAt 32 payload0

    (encryptedPublicKeyBytes, ciphertext) =
      ByteString.splitAt 60 payload1

  ephemeralPublicKey :: PublicKey <-
    decodePublicKey ephemeralPublicKeyBytes

  publicKeyBytes :: ByteString <-
    decrypt privateKey ephemeralPublicKey encryptedPublicKeyBytes

  publicKey :: PublicKey <-
    decodePublicKey publicKeyBytes

  plaintext :: ByteString <-
    decrypt privateKey publicKey ciphertext

  pure (publicKey, plaintext)


throwLeft :: Exception e => IO (Either e a) -> IO a
throwLeft action =
  action >>= either throwIO pure
