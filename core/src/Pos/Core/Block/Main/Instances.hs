{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Miscellaneous instances, etc. Related to the main blockchain of course.

module Pos.Core.Block.Main.Instances
       (
       ) where

import           Universum

import qualified Data.Text.Buildable as Buildable
import           Formatting (bprint, build, int, stext, (%))
import           Serokell.Util (Color (Magenta), colorize, listJson)

import           Pos.Binary.Class (Bi)
import           Pos.Core.Block.Blockchain (GenericBlock (..),
                     GenericBlockHeader (..))
import           Pos.Core.Block.Main.Lens (mainBlockBlockVersion,
                     mainBlockDifficulty, mainBlockSlot,
                     mainBlockSoftwareVersion, mainHeaderBlockVersion,
                     mainHeaderDifficulty, mainHeaderLeaderKey, mainHeaderSlot,
                     mainHeaderSoftwareVersion, mbTxs, mcdDifficulty,
                     mehBlockVersion, mehSoftwareVersion)
import           Pos.Core.Block.Main.Types (MainBody (..),
                     MainExtraHeaderData (..))
import           Pos.Core.Block.Union.Types (BlockHeader (..),
                     HasHeaderHash (..), HeaderHash, IsHeader,
                     IsMainHeader (..), MainBlock, MainBlockHeader,
                     MainConsensusData (..), blockHeaderHash,
                     headerHashHexF)
import           Pos.Core.Common (HasDifficulty (..))
import           Pos.Core.Slotting (EpochOrSlot (..), HasEpochIndex (..),
                     HasEpochOrSlot (..), slotIdF)
import           Pos.Core.Update (HasBlockVersion (..), HasSoftwareVersion (..))

instance NFData (MainBlock attr)

instance Bi (BlockHeader attr) =>
         Buildable (MainBlockHeader attr) where
    build gbh@UnsafeGenericBlockHeader {..} =
        bprint
            ("MainBlockHeader:\n"%
             "    hash: "%headerHashHexF%"\n"%
             "    previous block: "%headerHashHexF%"\n"%
             "    slot: "%slotIdF%"\n"%
             "    difficulty: "%int%"\n"%
             "    leader: "%build%"\n"%
             "    signature: "%build%"\n"%
             build
            )
            gbhHeaderHash
            _gbhPrevBlock
            _mcdSlot
            _mcdDifficulty
            _mcdLeaderKey
            _mcdSignature
            _gbhExtra
      where
        gbhHeaderHash :: HeaderHash
        gbhHeaderHash = blockHeaderHash $ BlockHeaderMain gbh
        MainConsensusData {..} = _gbhConsensus

instance Bi (BlockHeader attr) => Buildable (MainBlock attr) where
    build UnsafeGenericBlock {..} =
        bprint
            (stext%":\n"%
             "  "%build%
             "  transactions ("%int%" items): "%listJson%"\n"%
             "  "%build%"\n"%
             "  "%build%"\n"%
             "  update payload: "%build%"\n"%
             "  "%build
            )
            (colorize Magenta "MainBlock")
            _gbHeader
            (length txs)
            txs
            _mbDlgPayload
            _mbSscPayload
            _mbUpdatePayload
            _gbExtra
      where
        MainBody {..} = _gbBody
        txs = _gbBody ^. mbTxs

instance HasEpochIndex (MainBlock attr) where
    epochIndexL = mainBlockSlot . epochIndexL

instance HasEpochIndex (MainBlockHeader attr) where
    epochIndexL = mainHeaderSlot . epochIndexL

instance HasEpochOrSlot (MainBlockHeader attr) where
    getEpochOrSlot = EpochOrSlot . Right . view mainHeaderSlot

instance HasEpochOrSlot (MainBlock attr) where
    getEpochOrSlot = getEpochOrSlot . _gbHeader

instance Bi (BlockHeader attr) =>
         HasHeaderHash (MainBlockHeader attr) where
    headerHash = blockHeaderHash . BlockHeaderMain

instance Bi (BlockHeader attr) =>
         HasHeaderHash (MainBlock attr) where
    headerHash = blockHeaderHash . BlockHeaderMain . _gbHeader

instance HasDifficulty MainConsensusData where
    difficultyL = mcdDifficulty

instance HasDifficulty (MainBlockHeader attr) where
    difficultyL = mainHeaderDifficulty

instance HasDifficulty (MainBlock attr) where
    difficultyL = mainBlockDifficulty

instance HasBlockVersion MainExtraHeaderData where
    blockVersionL = mehBlockVersion

instance HasSoftwareVersion MainExtraHeaderData where
    softwareVersionL = mehSoftwareVersion

instance HasBlockVersion (MainBlock attr) where
    blockVersionL = mainBlockBlockVersion

instance HasSoftwareVersion (MainBlock attr) where
    softwareVersionL = mainBlockSoftwareVersion

instance HasBlockVersion (MainBlockHeader attr) where
    blockVersionL = mainHeaderBlockVersion

instance HasSoftwareVersion (MainBlockHeader attr) where
    softwareVersionL = mainHeaderSoftwareVersion

instance Bi (BlockHeader attr) => IsHeader (MainBlockHeader attr)

instance Bi (BlockHeader attr) => IsMainHeader (MainBlockHeader attr) where
    headerSlotL = mainHeaderSlot
    headerLeaderKeyL = mainHeaderLeaderKey
