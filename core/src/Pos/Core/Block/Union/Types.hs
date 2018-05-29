{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE StandaloneDeriving #-}

-- the Getter instances
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Union of blockchain types.

module Pos.Core.Block.Union.Types
       ( BlockHeader (BlockHeaderGenesis, BlockHeaderMain)
       , _BlockHeaderGenesis
       , _BlockHeaderMain
       , choosingBlockHeader
       , Block

       -- * GenesisBlockchain
       , GenesisBlockchain
       , GenesisBlockHeader
       , GenesisBlock

       -- * MainBlockchain
       , MainBlockchain
       , MainBlockHeader
       , MainBlock

       -- * MainConsensusData
       , MainConsensusData (..)
       , MainToSign (..)
       , BlockSignature (..)

       -- * HeaderHash related types and functions
       , HeaderHash (..)
       , anyHeaderHash
       , headerHashHexF
       , shortHeaderHashF
       , headerHashF
       , HasHeaderHash (..)
       , headerHashG
       , HasPrevBlock (..)

       , blockHeaderHash
       , blockHeaderProtocolMagic

       -- * IsHeader classes
       , IsHeader
       , IsGenesisHeader
       , IsMainHeader (..)

       , module Pos.Core.Block.Genesis.Types
       , module Pos.Core.Block.Main.Types
       ) where

import           Codec.CBOR.Decoding (decodeWordCanonical)
import           Codec.CBOR.Encoding (encodeWord)
import           Control.Lens (Getter, LensLike', choosing, makePrisms, to)

import           Data.SafeCopy (SafeCopy (..), contain, safeGet, safePut)
import qualified Data.Serialize as Cereal
import           Crypto.Hash (digestFromByteString)
import qualified Data.ByteArray as BA
import qualified Data.Text.Buildable as Buildable
import qualified Data.Text.Lazy.Builder as Builder
import           Formatting (Format, bprint, build, fitLeft, later, (%), (%.))
import           Universum

import           Pos.Binary.Class (Bi (..), DecoderAttrKind (..),
                     decodeListLenCanonicalOf, deserializeOrFail, encodeListLen,
                     enforceSize)
import           Pos.Core.Block.Blockchain (Blockchain (..), GenericBlock (..),
                     GenericBlockHeader (..), gbHeader, gbhPrevBlock)
import           Pos.Core.Block.Genesis.Types
import           Pos.Core.Block.Main.Types
import           Pos.Core.Common (ChainDifficulty, HasDifficulty (..))
import           Pos.Core.Delegation (ProxySigHeavy, ProxySigLight)
import           Pos.Core.Slotting (HasEpochIndex (..), HasEpochOrSlot (..),
                     SlotId (..))
import           Pos.Core.Ssc (mkSscProof)
import           Pos.Core.Txp (mkTxProof)
import           Pos.Core.Update (HasBlockVersion (..), HasSoftwareVersion (..),
                     mkUpdateProof)
import           Pos.Crypto (AbstractHash (..), Hash, ProtocolMagic, PublicKey,
                     Signature, hash)
import           Pos.Util.Some (Some, applySome, liftLensSome)
import           Pos.Util.Util (cborError, cerealError, toCborError,
                     toCerealError)
import           Unsafe.Coerce (unsafeCoerce)

----------------------------------------------------------------------------
-- GenesisBlockchain
----------------------------------------------------------------------------

-- | Represents blockchain consisting of genesis blocks.  Genesis
-- block doesn't have any special payload and is not strictly
-- necessary. However, it is good idea to store list of leaders
-- explicitly, because calculating it may be expensive operation. For
-- example, it is useful for SPV-clients.
data GenesisBlockchain

-- | Header of Genesis block.
type GenesisBlockHeader attr = GenericBlockHeader GenesisBlockchain attr

-- | Genesis block parametrized by 'GenesisBlockchain'.
type GenesisBlock attr = GenericBlock GenesisBlockchain attr

instance Blockchain GenesisBlockchain attr where
    type BodyProof GenesisBlockchain = GenesisProof
    type ConsensusData GenesisBlockchain = GenesisConsensusData
    type BBlockHeader GenesisBlockchain attr = Hash (BlockHeader attr)
    type BHeaderHash GenesisBlockchain = HeaderHash
    type ExtraHeaderData GenesisBlockchain = GenesisExtraHeaderData

    type Body GenesisBlockchain = GenesisBody

    type ExtraBodyData GenesisBlockchain = GenesisExtraBodyData
    type BBlock GenesisBlockchain attr = Block attr

    mkBodyProof = GenesisProof . hash . _gbLeaders

----------------------------------------------------------------------------
-- MainBlockchain
----------------------------------------------------------------------------

-- | Represents blockchain consisting of main blocks, i. e. blocks
-- with actual payload (transactions, SSC, update system, etc.).
data MainBlockchain

-- | Header of generic main block.
type MainBlockHeader attr = GenericBlockHeader MainBlockchain attr

-- | MainBlock is a block with transactions and MPC messages. It's the
-- main part of our consensus algorithm.
type MainBlock attr = GenericBlock MainBlockchain attr

-- | Signature of the block. Can be either regular signature from the
-- issuer or delegated signature having a constraint on epoch indices
-- (it means the signature is valid only if block's slot id has epoch
-- inside the constrained interval).
data BlockSignature
    = BlockSignature (Signature MainToSign)
    | BlockPSignatureLight (ProxySigLight MainToSign)
    | BlockPSignatureHeavy (ProxySigHeavy MainToSign)
    deriving (Show, Eq, Generic)

instance NFData MainProof => NFData BlockSignature

instance Buildable BlockSignature where
    build (BlockSignature s)       = bprint ("BlockSignature: "%build) s
    build (BlockPSignatureLight s) = bprint ("BlockPSignatureLight: "%build) s
    build (BlockPSignatureHeavy s) = bprint ("BlockPSignatureHeavy: "%build) s

instance Bi BlockSignature where
    encode input = case input of
        BlockSignature sig       -> encodeListLen 2 <> encode (0 :: Word8) <> encode sig
        BlockPSignatureLight pxy -> encodeListLen 2 <> encode (1 :: Word8) <> encode pxy
        BlockPSignatureHeavy pxy -> encodeListLen 2 <> encode (2 :: Word8) <> encode pxy
    decode = do
        enforceSize "BlockSignature" 2
        tag <- decode @Word8
        case tag of
          0 -> BlockSignature <$> decode
          1 -> BlockPSignatureLight <$> decode
          2 -> BlockPSignatureHeavy <$> decode
          _ -> cborError $ "decode@BlockSignature: unknown tag: " <> show tag

instance SafeCopy BlockSignature where
    getCopy = contain $ Cereal.getWord8 >>= \case
        0 -> BlockSignature <$> safeGet
        1 -> BlockPSignatureLight <$> safeGet
        2 -> BlockPSignatureHeavy <$> safeGet
        t -> cerealError $ "getCopy@BlockSignature: couldn't read tag: " <> show t
    putCopy (BlockSignature sig)            = contain $ Cereal.putWord8 0 >> safePut sig
    putCopy (BlockPSignatureLight proxySig) = contain $ Cereal.putWord8 1 >> safePut proxySig
    putCopy (BlockPSignatureHeavy proxySig) = contain $ Cereal.putWord8 2 >> safePut proxySig

-- | Data to be signed in main block.
data MainToSign
    = MainToSign
    { _msHeaderHash  :: !HeaderHash
      -- ^ Hash of previous header in the chain
    , _msBodyProof   :: !MainProof
    , _msSlot        :: !SlotId
    , _msChainDiff   :: !ChainDifficulty
    , _msExtraHeader :: !MainExtraHeaderData
    } deriving Generic

deriving instance Show MainToSign
deriving instance Eq MainToSign

instance Bi MainToSign where
    encode mts = encodeListLen 5
               <> encode (_msHeaderHash mts)
               <> encode (_msBodyProof mts)
               <> encode (_msSlot mts)
               <> encode (_msChainDiff mts)
               <> encode (_msExtraHeader mts)
    decode = do
        enforceSize "MainToSign" 5
        MainToSign <$> decode <*>
                          decode <*>
                          decode <*>
                          decode <*>
                          decode

data MainConsensusData = MainConsensusData
    { -- | Id of the slot for which this block was generated.
      _mcdSlot       :: !SlotId
    , -- | Public key of the slot leader. It's essential to have it here,
      -- because FTS gives us only hash of public key (aka 'StakeholderId').
      _mcdLeaderKey  :: !PublicKey
    , -- | Difficulty of chain ending in this block.
      _mcdDifficulty :: !ChainDifficulty
    , -- | Signature given by slot leader.
      _mcdSignature  :: !BlockSignature
    } deriving (Generic, Show, Eq)

instance NFData MainConsensusData

instance Bi MainConsensusData where
    encode cd =  encodeListLen 4
              <> encode (_mcdSlot cd)
              <> encode (_mcdLeaderKey cd)
              <> encode (_mcdDifficulty cd)
              <> encode (_mcdSignature cd)
    decode = do
        enforceSize "ConsensusData MainBlockchain)" 4
        MainConsensusData <$> decode <*>
                                 decode <*>
                                 decode <*>
                                 decode

instance ( Bi (BlockHeader attr)
         , Bi MainProof) =>
         Blockchain MainBlockchain attr where

    type BodyProof MainBlockchain = MainProof

    type ConsensusData MainBlockchain = MainConsensusData

    type BBlockHeader MainBlockchain attr = BlockHeader attr
    type BHeaderHash MainBlockchain = HeaderHash
    type ExtraHeaderData MainBlockchain = MainExtraHeaderData

    type Body MainBlockchain = MainBody

    type ExtraBodyData MainBlockchain = MainExtraBodyData
    type BBlock MainBlockchain attr = Block attr

    mkBodyProof MainBody{..} =
        MainProof
        { mpTxProof = mkTxProof _mbTxPayload
        , mpMpcProof = mkSscProof _mbSscPayload
        , mpProxySKsProof = hash _mbDlgPayload
        , mpUpdateProof = mkUpdateProof _mbUpdatePayload
        }

instance SafeCopy MainConsensusData where
    getCopy =
        contain $
        do _mcdSlot <- safeGet
           _mcdLeaderKey <- safeGet
           _mcdDifficulty <- safeGet
           _mcdSignature <- safeGet
           return $! MainConsensusData {..}
    putCopy MainConsensusData {..} =
        contain $
        do safePut _mcdSlot
           safePut _mcdLeaderKey
           safePut _mcdDifficulty
           safePut _mcdSignature

----------------------------------------------------------------------------
-- GenesisBlock ∪ MainBlock
----------------------------------------------------------------------------

-- | Either header of ordinary main block or genesis block.
data BlockHeader attr
    = BlockHeaderGenesis (GenesisBlockHeader attr)
    | BlockHeaderMain (MainBlockHeader attr)

deriving instance Generic (BlockHeader attr)
deriving instance (Eq (GenesisBlockHeader attr), Eq (MainBlockHeader attr)) => Eq (BlockHeader attr)
deriving instance (Show (GenesisBlockHeader attr), Show (MainBlockHeader attr)) => Show (BlockHeader attr)
instance
    ( NFData (GenesisBlockHeader attr)
    , NFData (MainBlockHeader attr)
    )
    => NFData (BlockHeader attr) where
    rnf (BlockHeaderGenesis header) = rnf header
    rnf (BlockHeaderMain header)    = rnf header

choosingBlockHeader :: Functor f =>
       LensLike' f (GenesisBlockHeader attr) r
    -> LensLike' f (MainBlockHeader attr) r
    -> LensLike' f (BlockHeader attr) r
choosingBlockHeader onGenesis onMain f = \case
    BlockHeaderGenesis bh -> BlockHeaderGenesis <$> onGenesis f bh
    BlockHeaderMain bh -> BlockHeaderMain <$> onMain f bh

instance Bi (BlockHeader 'AttrNone) where
   encode x = encodeListLen 2 <> encodeWord tag <> body
     where
       (tag, body) = case x of
         BlockHeaderGenesis bh -> (0, encode bh)
         BlockHeaderMain bh    -> (1, encode bh)

   decode = do
       decodeListLenCanonicalOf 2
       t <- decodeWordCanonical
       case t of
           0 -> BlockHeaderGenesis <$!> decode
           1 -> BlockHeaderMain <$!> decode
           _ -> cborError $ "decode@BlockHeader: unknown tag " <> pretty t

-- | Block.
type Block (attr :: DecoderAttrKind) = Either (GenesisBlock attr) (MainBlock attr)

----------------------------------------------------------------------------
-- HeaderHash
----------------------------------------------------------------------------

-- | 'Hash' of block header.
newtype HeaderHash = HeaderHash
    { unHeaderHash :: forall (attr :: DecoderAttrKind). Hash (BlockHeader attr) }

-- | Constructor for `HeaderHash`.
anyHeaderHash :: Hash (BlockHeader attr) -> HeaderHash
anyHeaderHash h = HeaderHash (unsafeCoerce h)

deriving instance Eq HeaderHash
deriving instance Show HeaderHash
deriving instance Ord HeaderHash
deriving instance BA.ByteArrayAccess HeaderHash
deriving instance Typeable HeaderHash
deriving instance NFData HeaderHash

instance Buildable HeaderHash where
    build (HeaderHash (AbstractHash h)) = Builder.fromString $ show h

-- TODO: add QuickCheck tests
instance SafeCopy HeaderHash where
    putCopy (HeaderHash h) = putCopy $ coerce h
        where
        coerce :: Hash (BlockHeader attr) -> Hash (BlockHeader 'AttrNone)
        coerce = unsafeCoerce

    getCopy = contain $ do
        bs <- safeGet
        toCerealError $ case deserializeOrFail @(Hash (BlockHeader 'AttrNone)) bs of
            Left (err, _) -> Left $ "getCopy@" <> label (Proxy @(Hash (BlockHeader 'AttrNone))) <> ": " <> show err
            Right (x, _) -> Right (anyHeaderHash x)

-- | Specialized formatter for 'HeaderHash'.
headerHashHexF :: Format r (HeaderHash -> r)
headerHashHexF = later $ \(HeaderHash (AbstractHash x)) -> Buildable.build (show x :: Text)

-- | Smart formatter for 'Hash' to show only first @8@ characters of 'Hash'.
shortHeaderHashF :: Format r (HeaderHash -> r)
shortHeaderHashF = fitLeft 8 %. headerHashHexF

-- |
-- `HeaderHash` `Bi` instance is compatible with `Bi` instance of `AbstractHash`
-- but doe not carry `Typeable` constraints.
instance Bi HeaderHash where
    encode (HeaderHash (AbstractHash digest))
        = encode (BA.convert digest :: ByteString)
    decode = do
        bs <- decode @ByteString
        toCborError $ case digestFromByteString bs of
            Nothing -> Left "HeaderHash.decode: invalid digest"
            Just x  -> Right (anyHeaderHash $ AbstractHash x)

-- | Specialized formatter for 'HeaderHash'.
headerHashF :: Format r (HeaderHash -> r)
headerHashF = build

-- HasHeaderHash
class HasHeaderHash a where
    headerHash :: a -> HeaderHash

instance HasHeaderHash HeaderHash where
    headerHash = identity

instance HasHeaderHash (Some HasHeaderHash) where
    headerHash = applySome headerHash

headerHashG :: HasHeaderHash a => Getter a HeaderHash
headerHashG = to headerHash

-- | This function is required because type inference fails in attempts to
-- hash only @Right@ or @Left@.
--
-- Perhaps, it shouldn't be here, but I decided not to create a module
-- for only this function.
blockHeaderHash :: forall attr. Bi (BlockHeader attr) => BlockHeader attr -> HeaderHash
blockHeaderHash = anyHeaderHash . hash

-- HasPrevBlock
-- | Class for something that has previous block (lens to 'Hash' for this block).
class HasPrevBlock s where
    prevBlockL :: Lens' s HeaderHash

instance HasPrevBlock (Some HasPrevBlock) where
    prevBlockL = liftLensSome prevBlockL

instance (HasPrevBlock s, HasPrevBlock s') =>
         HasPrevBlock (Either s s') where
    prevBlockL = choosing prevBlockL prevBlockL

-- Perhaps it is not the best instance.
instance {-# OVERLAPPABLE #-} HasPrevBlock s => HasPrevBlock (s, z) where
    prevBlockL = _1 . prevBlockL

instance (BHeaderHash b ~ HeaderHash) =>
         HasPrevBlock (GenericBlockHeader b attr) where
    prevBlockL = gbhPrevBlock

instance (BHeaderHash b ~ HeaderHash) =>
         HasPrevBlock (GenericBlock b attr) where
    prevBlockL = gbHeader . gbhPrevBlock

instance HasPrevBlock (BlockHeader attr) where
    prevBlockL = choosingBlockHeader prevBlockL prevBlockL


-- | The 'ProtocolMagic' in a 'BlockHeader'.
blockHeaderProtocolMagic :: BlockHeader attr -> ProtocolMagic
blockHeaderProtocolMagic (BlockHeaderGenesis gbh) = _gbhProtocolMagic gbh
blockHeaderProtocolMagic (BlockHeaderMain mbh)    = _gbhProtocolMagic mbh

makePrisms 'BlockHeaderGenesis

----------------------------------------------------------------------------
-- IsHeader
----------------------------------------------------------------------------

{- | A class that lets subpackages use some fields from headers without
depending on cardano-sl:

  * 'difficultyL'
  * 'epochIndexL'
  * 'epochOrSlotG'
  * 'prevBlockL'
  * 'headerHashG'
-}
class ( HasDifficulty header
      , HasEpochIndex header
      , HasEpochOrSlot header
      , HasPrevBlock header
      , HasHeaderHash header) =>
      IsHeader header

instance HasDifficulty (Some IsHeader) where
    difficultyL = liftLensSome difficultyL

instance HasEpochIndex (Some IsHeader) where
    epochIndexL = liftLensSome epochIndexL

instance HasEpochOrSlot (Some IsHeader) where
    getEpochOrSlot = applySome getEpochOrSlot

instance HasPrevBlock (Some IsHeader) where
    prevBlockL = liftLensSome prevBlockL

instance HasHeaderHash (Some IsHeader) where
    headerHash = applySome headerHash

instance IsHeader (Some IsHeader)

----------------------------------------------------------------------------
-- IsGenesisHeader
----------------------------------------------------------------------------

-- | A class for genesis headers.
class IsHeader header => IsGenesisHeader header

instance HasDifficulty (Some IsGenesisHeader) where
    difficultyL = liftLensSome difficultyL

instance HasEpochIndex (Some IsGenesisHeader) where
    epochIndexL = liftLensSome epochIndexL

instance HasEpochOrSlot (Some IsGenesisHeader) where
    getEpochOrSlot = applySome getEpochOrSlot

instance HasPrevBlock (Some IsGenesisHeader) where
    prevBlockL = liftLensSome prevBlockL

instance HasHeaderHash (Some IsGenesisHeader) where
    headerHash = applySome headerHash

instance IsHeader        (Some IsGenesisHeader)
instance IsGenesisHeader (Some IsGenesisHeader)

----------------------------------------------------------------------------
-- IsMainHeader
----------------------------------------------------------------------------

{- | A class for main headers. In addition to 'IsHeader', provides:

  * 'headerSlotL'
  * 'headerLeaderKeyL'
  * 'blockVersionL'
  * 'softwareVersionL'
-}
class (IsHeader header
      ,HasBlockVersion header
      ,HasSoftwareVersion header) =>
      IsMainHeader header
  where
    -- | Id of the slot for which this block was generated.
    headerSlotL :: Lens' header SlotId
    -- | Public key of slot leader.
    headerLeaderKeyL :: Lens' header PublicKey

instance HasDifficulty (Some IsMainHeader) where
    difficultyL = liftLensSome difficultyL

instance HasEpochIndex (Some IsMainHeader) where
    epochIndexL = liftLensSome epochIndexL

instance HasEpochOrSlot (Some IsMainHeader) where
    getEpochOrSlot = applySome getEpochOrSlot

instance HasPrevBlock (Some IsMainHeader) where
    prevBlockL = liftLensSome prevBlockL

instance HasHeaderHash (Some IsMainHeader) where
    headerHash = applySome headerHash

instance HasBlockVersion (Some IsMainHeader) where
    blockVersionL = liftLensSome blockVersionL

instance HasSoftwareVersion (Some IsMainHeader) where
    softwareVersionL = liftLensSome softwareVersionL

instance IsHeader     (Some IsMainHeader)
instance IsMainHeader (Some IsMainHeader) where
    headerSlotL = liftLensSome headerSlotL
    headerLeaderKeyL = liftLensSome headerLeaderKeyL
