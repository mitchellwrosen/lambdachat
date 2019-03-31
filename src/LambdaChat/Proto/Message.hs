module LambdaChat.Proto.Message where

import qualified Capnp.Gen.Protocol.Message.Pure as LambdaChat.Capnp

import Control.Monad.STE

import qualified Capnp


encodeProtoMessage :: LambdaChat.Capnp.Message -> Maybe ByteString
encodeProtoMessage message =
  runSTE
    (Capnp.evalLimitT Capnp.defaultLimit (Capnp.valueToBS message))
    (either (const Nothing) Just)

decodeProtoMessage :: ByteString -> Maybe LambdaChat.Capnp.Message
decodeProtoMessage bytes =
  Capnp.bsToValue bytes
