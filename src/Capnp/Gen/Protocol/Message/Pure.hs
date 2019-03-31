{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Capnp.Gen.Protocol.Message.Pure(Message(..)
                                      ,ChatMessage(..)) where
import qualified Capnp.GenHelpers.ReExports.Data.Vector as V
import qualified Capnp.GenHelpers.ReExports.Data.Text as T
import qualified Capnp.GenHelpers.ReExports.Data.ByteString as BS
import qualified Capnp.GenHelpers.ReExports.Data.Default as Default
import qualified GHC.Generics as Generics
import qualified Control.Monad.IO.Class as MonadIO
import qualified Capnp.Untyped.Pure as UntypedPure
import qualified Capnp.Untyped as Untyped
import qualified Capnp.Message as Message
import qualified Capnp.Classes as Classes
import qualified Capnp.Basics.Pure as BasicsPure
import qualified Capnp.GenHelpers.Pure as GenHelpersPure
import qualified Capnp.Gen.ById.Xfe94a8fd9f3ecbb7
import qualified Prelude as Std_
import qualified Data.Word as Std_
import qualified Data.Int as Std_
import Prelude ((<$>), (<*>), (>>=))
data Message 
    = Message 
        {chat :: ChatMessage}
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default Message) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg Message) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize Message) where
    type Cerial msg Message = (Capnp.Gen.ById.Xfe94a8fd9f3ecbb7.Message msg)
    decerialize raw = (Message <$> ((Capnp.Gen.ById.Xfe94a8fd9f3ecbb7.get_Message'chat raw) >>= Classes.decerialize))
instance (Classes.Marshal Message) where
    marshalInto raw_ value_ = case value_ of
        Message{..} ->
            (do
                ((Classes.cerialize (Untyped.message raw_) chat) >>= (Capnp.Gen.ById.Xfe94a8fd9f3ecbb7.set_Message'chat raw_))
                (Std_.pure ())
                )
instance (Classes.Cerialize Message)
instance (Classes.Cerialize (V.Vector Message)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize (V.Vector (V.Vector Message))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector Message)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector Message))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Message)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Message))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Message)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
data ChatMessage 
    = ChatMessage 
        {message :: BS.ByteString}
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default ChatMessage) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg ChatMessage) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize ChatMessage) where
    type Cerial msg ChatMessage = (Capnp.Gen.ById.Xfe94a8fd9f3ecbb7.ChatMessage msg)
    decerialize raw = (ChatMessage <$> ((Capnp.Gen.ById.Xfe94a8fd9f3ecbb7.get_ChatMessage'message raw) >>= Classes.decerialize))
instance (Classes.Marshal ChatMessage) where
    marshalInto raw_ value_ = case value_ of
        ChatMessage{..} ->
            (do
                ((Classes.cerialize (Untyped.message raw_) message) >>= (Capnp.Gen.ById.Xfe94a8fd9f3ecbb7.set_ChatMessage'message raw_))
                (Std_.pure ())
                )
instance (Classes.Cerialize ChatMessage)
instance (Classes.Cerialize (V.Vector ChatMessage)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize (V.Vector (V.Vector ChatMessage))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector ChatMessage)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector ChatMessage))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector ChatMessage)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector ChatMessage))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector ChatMessage)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec