{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

-- | Specification of Pos.Block and Pos.Block.Pure.

module Test.Pos.Block.BlockSpec
       ( spec
       ) where

import           Universum

import           Serokell.Util (VerificationRes (..), isVerSuccess)
import           Test.Hspec (Spec, describe, it)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck (Property, again, (===), (==>), (.&.))

import           Pos.Binary.Class (Bi, BiExtRep (..), DecoderAttrKind (..),
                     DecoderAttr (..), EitherExtRep (..), fillExtRep,
                     fillExtRep', serialize')
import qualified Pos.Block.Logic.Integrity as Block
import           Pos.Core (GenesisHash (..), HasConfiguration, genesisHash,
                     genesisHeaderHash)
import           Pos.Core (Blockchain (..), Block, BlockHeader (..),
                     BlockSignature (..), EpochIndex (..), GenericBlock (..),
                     GenericBlockHeader (..), GenesisBlockHeader,
                     GenesisBlockchain, GenesisBody (..),
                     GenesisConsensusData (..), GenesisExtraHeaderData (..),
                     HeavyDlgIndex (..), LightDlgIndices (..), MainBlockchain,
                     MainBlockHeader, MainBody (..), MainConsensusData (..),
                     MainExtraHeaderData (..), MainToSign (..), SlotId (..),
                     difficultyL, headerHash, mkBodyProof, mkGenericHeader',
                     mkGenesisHeader')
import           Pos.Core.Chrono (NewestFirst (..))
import           Pos.Core.Configuration (defaultCoreConfiguration,
                     withGenesisSpec)
import           Pos.Crypto (ProtocolMagic (..), ProxySecretKey (pskIssuerPk),
                     SecretKey, SignTag (..), createPsk, proxySign, sign,
                     toPublic)
import           Pos.Data.Attributes (mkAttributes)

import           Test.Pos.Block.Arbitrary as BT
import           Test.Pos.Crypto.Dummy (dummyProtocolMagic)


-- This tests are quite slow, hence max success is at most 20.
spec :: Spec
spec = do
    withGenesisSpec 0 defaultCoreConfiguration $ \_ -> do
        describe "Block properties" $ modifyMaxSuccess (min 20) $ do
            describe "mkMainHeader" $ do
                prop mainHeaderFormationDesc mainHeaderFormation
            describe "mkGenesisHeader" $ do
                prop genesisHeaderFormationDesc genesisHeaderFormation
            describe "verifyHeader" $ do
                prop verifyHeaderDesc validateGoodMainHeader
                prop invalidProtocolMagicHeaderDesc
                    validateBadProtocolMagicMainHeader
            describe "verifyHeaders" $ modifyMaxSuccess (const 1) $ do
                prop verifyHeadersDesc validateGoodHeaderChain
                emptyHeaderChain (NewestFirst [])
            describe "BiExtRep properties" $ do
                prop blockHeaderHashDesc $ again blockHeaderHashProp
                prop genericBlockExtRepDesc $ again
                    $ genericBlockExtRepProp @GenesisBlockchain
                    .&. genericBlockExtRepProp @MainBlockchain
                prop genericBlockHeaderInBlockDesc $ again
                    $ genericBlockHeaderInBlockProp @GenesisBlockchain
                    .&. genericBlockHeaderInBlockProp @MainBlockchain
        describe "BlockHeader properties" $ do
            describe "BiExtRep properties" $ do
                prop blockHeaderHeaderHashDesc $ again blockHeaderHeaderHashProp
                prop genericBlockHeaderExtRepDesc $ again
                    $ genericBlockHeaderExtRepProp @GenesisBlockchain @'AttrNone
                    .&. genericBlockHeaderExtRepProp @MainBlockchain @'AttrNone
  where
    mainHeaderFormationDesc
        = "Manually generating a main header block and using\
    \ mkMainHeader is the same"
    genesisHeaderFormationDesc
        = "Manually generating a genesis header block and using\
    \ mkGenesisHeader is the same"
    verifyHeaderDesc = "Successfully verifies a correct main block header"
    invalidProtocolMagicHeaderDesc =
        "Header with invalid protocol magic does not validate"
    verifyHeadersDesc =
        "Successfully verifies a correct chain of block headers"
    verifyEmptyHsDesc = "Successfully validates an empty header chain"
    blockHeaderHeaderHashDesc
        = "headerHash of `BlockHeader 'AttrNone` and `BlockHeader 'ExtRep`\
    \ should be equal"
    genericBlockHeaderExtRepDesc
        = "external representation of `BlockHeader 'AttrExtRep`\
    \ should be equal to serialized value of `BlockHeader 'AttrNone`"
    blockHeaderHashDesc
        = "headerHash of `Block 'AttrNone` and `Block 'AttrExtRep`\
    \ should be equal"
    genericBlockExtRepDesc
        = "external representation of `GenericBlock b 'AttrExtRep`\
    \ should be equal to serialized value of `GenericBlock b 'AttrNone`"
    genericBlockHeaderInBlockDesc
        = "external representation of `BlockHeader` from a `Block`\
    \ should be equal to serialized to its serialized value"

    emptyHeaderChain
        :: NewestFirst [] (BlockHeader 'AttrExtRep)
        -> Spec
    emptyHeaderChain l =
        it verifyEmptyHsDesc $ isVerSuccess $ Block.verifyHeaders dummyProtocolMagic Nothing l

-- | Both of the following tests are boilerplate - they use `mkGenericHeader` to create
-- headers and then compare these with manually built headers.
--
-- This is to keep vigilant over changes in the behavior of `mkGenericHeader` because of
-- the ensuing failed tests.

genesisHeaderFormation
    :: HasConfiguration
    => Maybe (BlockHeader 'AttrNone)
    -> EpochIndex
    -> GenesisBody
    -> Property
genesisHeaderFormation prevHeader epoch body = header === manualHeader
  where
    header = mkGenesisHeader'
        dummyProtocolMagic
        (maybe (Left (GenesisHash genesisHash)) Right prevHeader)
        epoch
        body
    manualHeader :: GenesisBlockHeader 'AttrNone
    manualHeader = UnsafeGenericBlockHeader
        { _gbhProtocolMagic = dummyProtocolMagic
        , _gbhPrevBlock     = h
        , _gbhBodyProof     = proof
        , _gbhConsensus     = consensus h proof
        , _gbhExtra         = GenesisExtraHeaderData $ mkAttributes ()
        , _gbhDecoderAttr = DecoderAttrNone
        }
    h          = maybe genesisHeaderHash headerHash prevHeader
    proof      = mkBodyProof @GenesisBlockchain body
    difficulty = maybe 0 (view difficultyL) prevHeader
    consensus _ _ = GenesisConsensusData
        { _gcdEpoch      = epoch
        , _gcdDifficulty = difficulty
        }

mainHeaderFormation
    :: HasConfiguration
    => Maybe (BlockHeader 'AttrNone)
    -> SlotId
    -> Either SecretKey (SecretKey, SecretKey, Bool)
    -> MainBody
    -> MainExtraHeaderData
    -> Property
mainHeaderFormation prevHeader slotId signer body extra =
    correctSigner signer ==> (header === manualHeader)
  where
    correctSigner (Left  _        ) = True
    correctSigner (Right (i, d, _)) = i /= d
    header :: MainBlockHeader 'AttrNone
    header = mkGenericHeader' @MainBlockchain dummyProtocolMagic
                                                 prevHash
                                                 body
                                                 consensus
                                                 extra
    manualHeader :: GenericBlockHeader MainBlockchain 'AttrNone
    manualHeader =
        UnsafeGenericBlockHeader
        { _gbhProtocolMagic = dummyProtocolMagic
        , _gbhPrevBlock = prevHash
        , _gbhBodyProof = proof
        , _gbhConsensus = consensus proof
        , _gbhExtra = extra
        , _gbhDecoderAttr = DecoderAttrNone
        }
    prevHash = maybe genesisHeaderHash headerHash prevHeader
    proof = mkBodyProof @MainBlockchain body
    (sk, pSk) = either (, Nothing) mkProxySk signer
    mkProxySk (issuerSK, delegateSK, isSigEpoch) =
        let epoch = siEpoch slotId
            delegatePK = toPublic delegateSK
            curried :: Bi w => w -> ProxySecretKey w
            curried = createPsk dummyProtocolMagic issuerSK delegatePK
            proxy =
                if isSigEpoch
                    then Right $ curried $ HeavyDlgIndex epoch
                    else Left $ curried $ LightDlgIndices (epoch, epoch)
        in (delegateSK, Just $ proxy)
    difficulty = maybe 0 (succ . view difficultyL) prevHeader
    makeSignature toSign (Left psk) =
        BlockPSignatureLight $ proxySign dummyProtocolMagic SignMainBlockLight sk psk toSign
    makeSignature toSign (Right psk) =
        BlockPSignatureHeavy $ proxySign dummyProtocolMagic SignMainBlockHeavy sk psk toSign
    signature p =
        let toSign = MainToSign prevHash p slotId difficulty extra
        in maybe
               (BlockSignature (sign dummyProtocolMagic SignMainBlock sk toSign))
               (makeSignature toSign)
               pSk
    consensus p =
        MainConsensusData
        { _mcdSlot = slotId
        , _mcdLeaderKey =
              maybe (toPublic sk) (either pskIssuerPk pskIssuerPk) pSk
        , _mcdDifficulty = difficulty
        , _mcdSignature = signature p
        }

----------------------------------------------------------------------------
-- GenesisBlock ∪ MainBlock
----------------------------------------------------------------------------

validateGoodMainHeader :: BT.HeaderAndParams -> Bool
validateGoodMainHeader (BT.getHAndP -> (params, header)) =
    isVerSuccess $ Block.verifyHeader dummyProtocolMagic params header

-- FIXME should sharpen this test to ensure that it fails with the expected
-- reason.
validateBadProtocolMagicMainHeader :: BT.HeaderAndParams -> Bool
validateBadProtocolMagicMainHeader (BT.getHAndP -> (params, header)) =
    let protocolMagic' = ProtocolMagic (getProtocolMagic dummyProtocolMagic + 1)
        header' = case header of
            BlockHeaderGenesis h -> BlockHeaderGenesis (h { _gbhProtocolMagic = protocolMagic' })
            BlockHeaderMain h    -> BlockHeaderMain    (h { _gbhProtocolMagic = protocolMagic' })
    in  not $ isVerSuccess $ Block.verifyHeader dummyProtocolMagic params header'

validateGoodHeaderChain :: BT.BlockHeaderList -> Property
validateGoodHeaderChain (BT.BHL (headers, _)) =
    -- Throw an error if `fillExtRep` fails.
    let res = Block.verifyHeaders dummyProtocolMagic Nothing (NewestFirst $ map (either (error . ("fillExtRep: " <>)) identity . fillExtRep) $ headers)
    in res === VerSuccess

-- This property is implied by `blockHeaderExtRepProp`.
blockHeaderHeaderHashProp :: BlockHeader 'AttrNone -> Property
blockHeaderHeaderHashProp bh = 
    let bhExtRep = fillExtRep bh
    in case bhExtRep of
        Left  _   -> error "fillExtRep failed"
        Right bh' -> headerHash bh' === headerHash bh

-- |
-- External representation of `GenericBlockHeader b 'AttrExtRep` should be equal to
-- serialized value of `BlockHeader 'AttrNone`.
genericBlockHeaderExtRepProp
    :: forall b attr .
       ( Typeable b
       , Bi (BHeaderHash b)
       , Bi (BodyProof b)
       , Bi (ConsensusData b)
       , Bi (ExtraHeaderData b)
       )
    =>  GenericBlockHeader b attr
    -> Property
genericBlockHeaderExtRepProp gbh =
    let gbhExtRep :: Either Text (GenericBlockHeader b 'AttrExtRep, ByteString)
        gbhExtRep = case _gbhDecoderAttr gbh of
            DecoderAttrNone         -> fillExtRep' gbh
            DecoderAttrOffsets _ _  -> fillExtRep' $ forgetExtRep gbh
            DecoderAttrExtRep _     -> Right (gbh, serialize' $ forgetExtRep gbh)
    in case gbhExtRep of
        Left  err       ->
            error $ "fillExtRep failed: " <> err
        Right (gbh', bs) ->
            bs === case _gbhDecoderAttr gbh' of
                DecoderAttrExtRep bs' -> bs'

blockFillExtRep' :: Block 'AttrNone -> Either Text (Block 'AttrExtRep, ByteString)
blockFillExtRep' = fmap (first runEitherExtRep) . fillExtRep' . EitherExtRep

-- |
-- Check `genericBlockHeaderExtRepProp` property for a header of a generic block.
genericBlockHeaderInBlockProp
    :: forall b .
       ( Typeable b
       , Bi (BHeaderHash b)
       , Bi (Body b)
       , Bi (ExtraBodyData b)
       , Bi (BodyProof b)
       , Bi (ConsensusData b)
       , Bi (ExtraHeaderData b)
       )
    => GenericBlock b 'AttrNone
    -> Property
genericBlockHeaderInBlockProp gb =
    case fillExtRep gb of
        Left _        ->
            error "fillExtRep failed"
        Right gb' ->
            genericBlockHeaderExtRepProp $ _gbHeader gb'

-- |
-- Header hashe of `Block attr` should be indepenend of `attr`.
blockHeaderHashProp :: Block 'AttrNone -> Property
blockHeaderHashProp b =
    let bExtRep = blockFillExtRep' b
    in case bExtRep of 
        Left _         ->
            error "fillExtRep failed"
        Right (b', _) ->
            headerHash b === headerHash b'

-- |
-- External representation of `Block 'AttrExtRep` should be equal to serialized
-- value of `Block 'AttrNone`.
genericBlockExtRepProp
    :: forall b . 
       ( Typeable b
       , Bi (BHeaderHash b)
       , Bi (Body b)
       , Bi (ExtraBodyData b)
       , Bi (BodyProof b)
       , Bi (ConsensusData b)
       , Bi (ExtraHeaderData b)
       )
    => GenericBlock b 'AttrNone
    -> Property
genericBlockExtRepProp gb =
    let gbExtRep = fillExtRep' gb
    in case gbExtRep of
        Left  _        ->
            error "fillExtRep failed"
        Right (gb', bs) ->
            bs === case _gbDecoderAttr gb' of
                DecoderAttrExtRep bs' -> bs'
