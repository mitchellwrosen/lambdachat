module LambdaChat.Server.Effect.ChatStorage
  ( ChatStorage
  ) where

import LambdaChat.Server.Chat

import Data.Time (UTCTime)


data ChatStorage :: (Type -> Type) -> Type -> Type where
  Store ::
       Chat
    -> k
    -> ChatStorage m k

  Search ::
       UTCTime
    -> ([Chat] -> k)
    -> ChatStorage m k
