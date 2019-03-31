{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Capnp.Gen.Protocol.Message where
import qualified Capnp.Message as Message
import qualified Capnp.Untyped as Untyped
import qualified Capnp.Basics as Basics
import qualified Capnp.GenHelpers as GenHelpers
import qualified Capnp.Classes as Classes
import qualified GHC.Generics as Generics
import qualified Capnp.Bits as Std_
import qualified Data.Maybe as Std_
import qualified Capnp.GenHelpers.ReExports.Data.ByteString as BS
import qualified Prelude as Std_
import qualified Data.Word as Std_
import qualified Data.Int as Std_
import Prelude ((<$>), (<*>), (>>=))
newtype Message msg
    = Message'newtype_ (Untyped.Struct msg)
instance (Untyped.TraverseMsg Message) where
    tMsg f (Message'newtype_ s) = (Message'newtype_ <$> (Untyped.tMsg f s))
instance (Classes.FromStruct msg (Message msg)) where
    fromStruct struct = (Std_.pure (Message'newtype_ struct))
instance (Classes.ToStruct msg (Message msg)) where
    toStruct (Message'newtype_ struct) = struct
instance (Untyped.HasMessage (Message msg)) where
    type InMessage (Message msg) = msg
    message (Message'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Message msg)) where
    messageDefault msg = (Message'newtype_ (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (Message msg)) where
    fromPtr msg ptr = (Message'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (Message (Message.MutMsg s))) where
    toPtr msg (Message'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (Message (Message.MutMsg s))) where
    new msg = (Message'newtype_ <$> (Untyped.allocStruct msg 0 1))
instance (Basics.ListElem msg (Message msg)) where
    newtype List msg (Message msg)
        = Message'List_ (Untyped.ListOf msg (Untyped.Struct msg))
    listFromPtr msg ptr = (Message'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (Message'List_ l) = (Untyped.ListStruct l)
    length (Message'List_ l) = (Untyped.length l)
    index i (Message'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (Message (Message.MutMsg s))) where
    setIndex (Message'newtype_ elt) i (Message'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (Message'List_ <$> (Untyped.allocCompositeList msg 0 1 len))
get_Message'chat :: ((Untyped.ReadCtx m msg)) => (Message msg) -> (m (ChatMessage msg))
get_Message'chat (Message'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 0 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Message'chat :: ((Untyped.RWCtx m s)) => (Message (Message.MutMsg s)) -> (ChatMessage (Message.MutMsg s)) -> (m ())
set_Message'chat (Message'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 0 struct)
    )
has_Message'chat :: ((Untyped.ReadCtx m msg)) => (Message msg) -> (m Std_.Bool)
has_Message'chat (Message'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 0 struct))
new_Message'chat :: ((Untyped.RWCtx m s)) => (Message (Message.MutMsg s)) -> (m (ChatMessage (Message.MutMsg s)))
new_Message'chat struct = (do
    result <- (Classes.new (Untyped.message struct))
    (set_Message'chat struct result)
    (Std_.pure result)
    )
newtype ChatMessage msg
    = ChatMessage'newtype_ (Untyped.Struct msg)
instance (Untyped.TraverseMsg ChatMessage) where
    tMsg f (ChatMessage'newtype_ s) = (ChatMessage'newtype_ <$> (Untyped.tMsg f s))
instance (Classes.FromStruct msg (ChatMessage msg)) where
    fromStruct struct = (Std_.pure (ChatMessage'newtype_ struct))
instance (Classes.ToStruct msg (ChatMessage msg)) where
    toStruct (ChatMessage'newtype_ struct) = struct
instance (Untyped.HasMessage (ChatMessage msg)) where
    type InMessage (ChatMessage msg) = msg
    message (ChatMessage'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (ChatMessage msg)) where
    messageDefault msg = (ChatMessage'newtype_ (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (ChatMessage msg)) where
    fromPtr msg ptr = (ChatMessage'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (ChatMessage (Message.MutMsg s))) where
    toPtr msg (ChatMessage'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (ChatMessage (Message.MutMsg s))) where
    new msg = (ChatMessage'newtype_ <$> (Untyped.allocStruct msg 0 1))
instance (Basics.ListElem msg (ChatMessage msg)) where
    newtype List msg (ChatMessage msg)
        = ChatMessage'List_ (Untyped.ListOf msg (Untyped.Struct msg))
    listFromPtr msg ptr = (ChatMessage'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (ChatMessage'List_ l) = (Untyped.ListStruct l)
    length (ChatMessage'List_ l) = (Untyped.length l)
    index i (ChatMessage'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (ChatMessage (Message.MutMsg s))) where
    setIndex (ChatMessage'newtype_ elt) i (ChatMessage'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (ChatMessage'List_ <$> (Untyped.allocCompositeList msg 0 1 len))
get_ChatMessage'message :: ((Untyped.ReadCtx m msg)) => (ChatMessage msg) -> (m (Basics.Data msg))
get_ChatMessage'message (ChatMessage'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 0 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_ChatMessage'message :: ((Untyped.RWCtx m s)) => (ChatMessage (Message.MutMsg s)) -> (Basics.Data (Message.MutMsg s)) -> (m ())
set_ChatMessage'message (ChatMessage'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 0 struct)
    )
has_ChatMessage'message :: ((Untyped.ReadCtx m msg)) => (ChatMessage msg) -> (m Std_.Bool)
has_ChatMessage'message (ChatMessage'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 0 struct))
new_ChatMessage'message :: ((Untyped.RWCtx m s)) => Std_.Int -> (ChatMessage (Message.MutMsg s)) -> (m (Basics.Data (Message.MutMsg s)))
new_ChatMessage'message len struct = (do
    result <- (Basics.newData (Untyped.message struct) len)
    (set_ChatMessage'message struct result)
    (Std_.pure result)
    )