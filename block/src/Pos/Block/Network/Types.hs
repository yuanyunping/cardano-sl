-- | Types describing protocol messages related to Blocks.

module Pos.Block.Network.Types
       ( MsgGetHeaders (..)
       , MsgGetBlocks (..)
       , MsgHeaders (..)
       , MsgBlock (..)
       , MsgSerializedBlock (..)
       , MsgStream (..)
       , MsgStreamStart (..)
       , MsgStreamUpdate (..)
       , MsgStreamBlock (..)
       ) where

import qualified Data.Text.Buildable
import           Formatting (bprint, build, (%))
import           Serokell.Util.Text (listJson)
import           Universum

import           Pos.Binary.Class (Bi (..), BiExtRep (..), Cons (..),
                     DecoderAttrKind (..), Field (..), deriveSimpleBi,
                     encodeListLen, enforceSize, runEitherExtRep,
                     runNonEmptyExtRep)
import           Pos.Core (HeaderHash)
import           Pos.Core.Block (Block, BlockHeader (..))
import           Pos.Core.Chrono (NE, NewestFirst (..))
import           Pos.DB.Class (SerializedBlock)
import           Pos.Util.Util (cborError)

-- | 'GetHeaders' message. Behaviour of the response depends on
-- particular combination of 'mghFrom' and 'mghTo'.
--
-- * 'mghTo' resolves to some header (let's call it @top@ for
-- convenience) -- node's tip if it's @Nothing@, header with hash in
-- @Just@ if it's @Just@.
--
-- * If 'mghFrom' is empty, then semantics is "request to return
-- header of block @top@".
--
-- * Otherwise (if 'mghFrom' isn't empty) it represents the set of
-- checkpoints. Responding node will try to iterate headers from @top@
-- to older until it reaches any checkpoint. If it finds checkpoint
-- @c@, it returns all headers in range @[c.next..top]@. If it doesn't
-- find any checkpoint or depth of searching exceeds
-- 'recoveryHeadersMessage', it will try to find the newest checkpoint
-- @cc@ from 'mghFrom' that's in main chain of responding node and
-- then return at most 'recoveryHeadersMessage' headers starting with
-- @cc@ as the oldest one, returning headers in range @l2 =
-- [cc.next..x]@ where @x@ is either @top@ (in case @length l2 <
-- recoveryHeadersMessage@) or some arbitrary header (and length is
-- precisely 'recoveryHeadersMessage').
data MsgGetHeaders = MsgGetHeaders
    { -- not guaranteed to be in any particular order
      mghFrom :: ![HeaderHash]
    , mghTo   :: !(Maybe HeaderHash)
    } deriving (Generic, Show, Eq)

instance Buildable MsgGetHeaders where
    build (MsgGetHeaders mghFrom mghTo) =
        bprint ("MsgGetHeaders {from = "%listJson%", to = "%build%"}")
               mghFrom (maybe "<Nothing>" (bprint build) mghTo)

deriveSimpleBi ''MsgGetHeaders [
    Cons 'MsgGetHeaders [
        Field [| mghFrom :: [HeaderHash]     |],
        Field [| mghTo   :: Maybe HeaderHash |]
    ]]

-- | 'GetBlocks' message (see protocol specification).
data MsgGetBlocks = MsgGetBlocks
    { mgbFrom :: !HeaderHash
    , mgbTo   :: !HeaderHash
    } deriving (Generic, Show, Eq)

instance Buildable MsgGetBlocks where
    build (MsgGetBlocks mgbFrom mgbTo) =
        bprint ("MsgGetBlocks {from = "%build%", to = "%build%"}")
               mgbFrom mgbTo

deriveSimpleBi ''MsgGetBlocks [
    Cons 'MsgGetBlocks [
        Field [| mgbFrom :: HeaderHash |],
        Field [| mgbTo   :: HeaderHash |]
    ]]

-- | 'Headers' message (see protocol specification).
data MsgHeaders attr
    = MsgHeaders (NewestFirst NE (BlockHeader attr))
    | MsgNoHeaders Text
    deriving (Eq, Show, Generic)

instance Bi (MsgHeaders 'AttrNone) where
    encode = \case
        (MsgHeaders b) -> encodeListLen 2 <> encode (0 :: Word8) <> encode b
        (MsgNoHeaders t) -> encodeListLen 2 <> encode (1 :: Word8) <> encode t
    decode = do
        enforceSize "MsgHeaders" 2
        tag <- decode @Word8
        case tag of
            0 -> MsgHeaders <$> decode
            1 -> MsgNoHeaders <$> decode
            t -> cborError $ "MsgHeaders wrong tag: " <> show t

instance BiExtRep MsgHeaders where
    decodeWithOffsets = do
        enforceSize "MsgHeaders" 2
        tag <- decode @Word8
        case tag of
            0 -> MsgHeaders . NewestFirst . runNonEmptyExtRep <$> decodeWithOffsets
            1 -> MsgNoHeaders <$> decode
            t -> cborError $ "MsgHeaders wrong tag: " <> show t
    spliceExtRep bs (MsgHeaders hs)  = MsgHeaders $ fmap (spliceExtRep bs) hs
    spliceExtRep _  (MsgNoHeaders t) = MsgNoHeaders t
    forgetExtRep (MsgHeaders hs)  = MsgHeaders $ fmap forgetExtRep hs
    forgetExtRep (MsgNoHeaders t) = MsgNoHeaders t

-- | 'Block' message (see protocol specification).
data MsgBlock attr
    = MsgBlock (Block attr)
    | MsgNoBlock Text
    deriving (Eq, Show, Generic)

instance Bi (MsgBlock 'AttrNone) where
    encode = \case
        MsgBlock b -> encodeListLen 2 <> encode (0 :: Word8) <> encode b
        MsgNoBlock t -> encodeListLen 2 <> encode (1 :: Word8) <> encode t
    decode = do
        enforceSize "MsgBlock" 2
        tag <- decode @Word8
        case tag of
            0 -> MsgBlock <$> decode
            1 -> MsgNoBlock <$> decode
            t -> cborError $ "MsgBlock wrong tag: " <> show t

instance BiExtRep MsgBlock where
    decodeWithOffsets = do
        enforceSize "MsgBlock" 2
        tag <- decode @Word8
        case tag of
            0 -> MsgBlock . runEitherExtRep <$> decodeWithOffsets
            1 -> MsgNoBlock <$> decode
            t -> cborError $ "MsgBlock wrong tag: " <> show t
    spliceExtRep bs (MsgBlock blk) = MsgBlock $ bimap (spliceExtRep bs) (spliceExtRep bs) blk
    spliceExtRep _  (MsgNoBlock t) = MsgNoBlock t
    forgetExtRep (MsgBlock blk) = MsgBlock $ bimap forgetExtRep forgetExtRep blk
    forgetExtRep (MsgNoBlock t) = MsgNoBlock t

-- | 'SerializedBlock' message
data MsgSerializedBlock
    = MsgSerializedBlock SerializedBlock
    | MsgNoSerializedBlock Text
    deriving (Generic)

data MsgStreamStart = MsgStreamStart
    { mssFrom   :: ![HeaderHash]
    , mssTo     :: !HeaderHash
    , mssWindow :: !Word32
    } deriving (Generic, Show, Eq)

deriveSimpleBi ''MsgStreamStart [
    Cons 'MsgStreamStart [
        Field [| mssFrom   :: [HeaderHash] |],
        Field [| mssTo     :: HeaderHash |],
        Field [| mssWindow :: Word32 |]
    ]]

data MsgStreamUpdate = MsgStreamUpdate
    { msuWindow :: !Word32
    } deriving (Generic, Show, Eq)

deriveSimpleBi ''MsgStreamUpdate [
    Cons 'MsgStreamUpdate [
        Field [| msuWindow :: Word32 |]
    ]]

data MsgStream
    = MsgStart MsgStreamStart
    | MsgUpdate MsgStreamUpdate
    deriving (Eq, Show, Generic)

instance Bi MsgStream where
    encode = \case
        MsgStart s  -> encodeListLen 2 <> encode (0 :: Word8) <> encode s
        MsgUpdate u -> encodeListLen 2 <> encode (1 :: Word8) <> encode u
    decode = do
        enforceSize "MsgStream" 2
        tag <- decode @Word8
        case tag of
            0 -> MsgStart  <$> decode
            1 -> MsgUpdate <$> decode
            t -> cborError $ "MsgStream wrong tag: " <> show t

data MsgStreamBlock attr
    = MsgStreamBlock (Block attr)
    | MsgStreamNoBlock Text
    | MsgStreamEnd
    deriving (Eq, Show, Generic)

instance Bi (MsgStreamBlock 'AttrNone) where
    encode = \case
        MsgStreamBlock b -> encodeListLen 2 <> encode (0 :: Word8) <> encode b
        MsgStreamNoBlock t -> encodeListLen 2 <> encode (1 :: Word8) <> encode t
        MsgStreamEnd -> encodeListLen 2 <> encode (2 :: Word8) <> encode (0 :: Word8)
    decode = do
        enforceSize "MsgBlock" 2
        tag <- decode @Word8
        case tag of
            0 -> MsgStreamBlock <$> decode
            1 -> MsgStreamNoBlock <$> decode
            2 -> do
                 (_ :: Word8 )<- decode
                 pure MsgStreamEnd
            t -> cborError $ "MsgStreamBlock wrong tag: " <> show t

instance BiExtRep MsgStreamBlock where
    decodeWithOffsets = do
        enforceSize "MsgBlock" 2
        tag <- decode @Word8
        case tag of
            0 -> MsgStreamBlock . runEitherExtRep <$> decodeWithOffsets
            1 -> MsgStreamNoBlock <$> decode
            2 -> do
                 (_ :: Word8 )<- decode
                 pure MsgStreamEnd
            t -> cborError $ "MsgStreamBlock wrong tag: " <> show t
    spliceExtRep bs (MsgStreamBlock blk) = MsgStreamBlock $ bimap (spliceExtRep bs) (spliceExtRep bs) blk
    spliceExtRep _  (MsgStreamNoBlock t) = MsgStreamNoBlock t
    spliceExtRep _  MsgStreamEnd         = MsgStreamEnd
    forgetExtRep (MsgStreamBlock blk) = MsgStreamBlock $ bimap forgetExtRep forgetExtRep blk
    forgetExtRep (MsgStreamNoBlock t) = MsgStreamNoBlock t
    forgetExtRep MsgStreamEnd         = MsgStreamEnd
