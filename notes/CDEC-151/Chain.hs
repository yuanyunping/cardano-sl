{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
module Chain
    ( ChainFragment (..)
    , Chain
    , emptyChain
    , fromList
    , ChainMeasure (..)
    , lookupBySlot
    , lookupByIndexFromEnd
    , splitBeforeSlot
    , findIntersection
    , findNext

    , addBlock
    , drop
    , take
    , append
    , length
    , pointOnChain

    , applyChainUpdate
    , applyChainUpdates
    , invReaderStates

    , chainHead
    , chainHeadBlockId
    , chainHeadSlot
    , chainBackwardsFrom

    -- testing
    , reifyChainFragment
    , absChainFragment
    , genChain
    , validChain
    , validChainFragment
    , chain

    , TestChain (..)
    , prop_addBlock
    , prop_drop
    , prop_take
    , prop_append
    , prop_reifyChainFragment
    , prop_absChainFragment
    , invChain
    , prop_TestChain

    )
    where

import           Prelude hiding (drop, length, take)

import           Data.FingerTree (FingerTree, Measured (..), SearchResult (..), ViewL (..),
                                  ViewR (..), (<|), (|>))
import qualified Data.FingerTree as FT
import qualified Data.List as L

import           Test.QuickCheck hiding ((><))

import           Block (Block (..), BlockId, ChainMeasure (..), Point, Slot, blockPoint)
import qualified Chain.Abstract as Chain.Abs
import           Chain.Update (ChainUpdate (..))

--
-- Blockchain fragment data type.
--

-- |
-- The chain grows to the right, it should never contain a block with slot `0`
-- (it will not be possible to find it with `lookupBySlot`, since `minBound
-- @Word == 0`.
newtype ChainFragment = ChainFragment (FingerTree ChainMeasure Block)
  deriving (Show, Eq)

type Chain = ChainFragment

emptyChain :: Chain
emptyChain = ChainFragment FT.empty

fromList :: [Block] -> ChainFragment
fromList = ChainFragment . FT.fromList

-- |
-- It assumes the chain is growing to the right.
lookupBySlot :: ChainFragment -> Slot -> FT.SearchResult ChainMeasure Block
lookupBySlot (ChainFragment t) s =
    FT.search (\vl vr -> maxSlot vl >= s && minSlot vr >= s) t

chain :: ChainFragment
chain = ChainFragment $ FT.fromList [Block 1 0 1 "", Block 2 1 2 "", Block 3 2 3 "", Block 4 3 10 "", Block 5 4 20 ""]

lookupByIndexFromEnd :: ChainFragment -> Int -> FT.SearchResult ChainMeasure Block
lookupByIndexFromEnd (ChainFragment t) n =
    FT.search (\vl vr -> size vl >= len - n && size vr <= n) t
  where
    len = size (measure t)

-- |
-- Find next block after the given point
findNext :: Point -> ChainFragment -> Maybe Block
findNext p cf = case lookupBySlot cf (fst p) of
    Position _ b ft'
        | blockPoint b == p
        -> case FT.viewl ft' of
            n :< _ -> Just n
            EmptyL -> Nothing
    _ -> Nothing

splitBeforeSlot :: ChainFragment -> Slot -> (ChainFragment, ChainFragment)
splitBeforeSlot (ChainFragment t) s =
    (\(l, r) -> (ChainFragment l, ChainFragment r))
  $ FT.split (\v -> maxSlot v >= s) t

findIntersection :: Chain -> Point -> [Point] -> Maybe (Point, Point)
findIntersection c hpoint points =
    go hpoint (hpoint : points)
    where
    go _ [] = Nothing
    go p (p':ps)
        | pointOnChain c p' = Just (p', p)
        | otherwise         = go p' ps

addBlock :: Chain -> Block -> Chain
addBlock (ChainFragment ft) b = ChainFragment (ft |> b)

prop_addBlock :: Block -> Chain.Abs.Chain -> Bool
prop_addBlock b c =
    b : c == absChainFragment (reifyChainFragment c `addBlock` b)

drop :: Int -> Chain -> Chain
drop n (ChainFragment ft) = ChainFragment $ FT.dropUntil (\v -> size v > n) ft

prop_drop :: Int -> Chain.Abs.Chain -> Bool
prop_drop n c =
    L.drop n c == absChainFragment (drop n $ reifyChainFragment c)

take :: Int -> Chain -> Chain
take n (ChainFragment ft) = ChainFragment $ FT.takeUntil (\v -> size v > n) ft

prop_take :: Int -> Chain.Abs.Chain -> Bool
prop_take n c =
    L.take n c == absChainFragment (take n $ reifyChainFragment c)

append :: [Block] -> Chain -> Chain
append bs (ChainFragment r) = ChainFragment (L.foldl' (|>) r bs)

length :: ChainFragment -> Int
length (ChainFragment ft) = size (measure ft)

pointOnChain :: Chain -> Point -> Bool
pointOnChain (ChainFragment ft) p = go ft
    where
    -- recursivelly search the fingertree from the right
    go t = case FT.viewr t of
        EmptyR                      -> False
        t' :> b | blockPoint b == p -> True
                | otherwise         -> go t'

prop_append :: [Block] -> Chain.Abs.Chain -> Bool
prop_append l r =
    l ++ r == absChainFragment (l `append` reifyChainFragment r)

chainHead :: Chain -> Maybe Block
chainHead (ChainFragment ft) = case FT.viewl ft of
    EmptyL -> Nothing
    b :< _ -> Just b

chainHeadBlockId :: Chain -> BlockId
chainHeadBlockId = maybe 0 blockId . chainHead

chainHeadSlot :: Chain -> Slot
chainHeadSlot = maybe 0 blockSlot . chainHead

-- This is the key operation on chains in this model
applyChainUpdate :: ChainUpdate -> Chain -> Chain
applyChainUpdate (AddBlock b) c = c `addBlock` b
applyChainUpdate (RollBack p) (ChainFragment c) = ChainFragment $ go c
    where
    go v = case FT.viewr v of
        EmptyR  -> v
        v' :> b | blockPoint b == p -> v
                | otherwise         -> go v'

applyChainUpdates :: [ChainUpdate] -> Chain -> Chain
applyChainUpdates = flip (foldl (flip applyChainUpdate))

-- like 'Chain.Volatile.invReaderState'
invReaderStates :: Chain -> readerState -> Bool
invReaderStates = undefined

chainBackwardsFrom :: Chain -> BlockId -> Chain
chainBackwardsFrom c bid = go c
    where
    go :: Chain -> Chain
    go c@(ChainFragment ft) = case FT.viewr ft of
        EmptyR   -> c
        ft' :> b | blockId b == bid -> ChainFragment (ft' |> b)
                 | otherwise        -> go (ChainFragment ft')

reifyChainFragment :: Chain.Abs.ChainFragment -> ChainFragment
reifyChainFragment = fromList . reverse

prop_reifyChainFragment :: Chain.Abs.ChainFragment -> Bool
prop_reifyChainFragment bs =
    absChainFragment (reifyChainFragment bs) == bs

-- |
-- Note the `foldl'`, this is that it's easy to append new blocks to the
-- abstract representation.
absChainFragment :: ChainFragment -> Chain.Abs.ChainFragment
absChainFragment (ChainFragment ft) = L.foldl' (flip (:)) [] ft

prop_absChainFragment :: ChainFragment -> Bool
prop_absChainFragment cf =
    reifyChainFragment (absChainFragment cf) == cf

validChain :: Chain -> Bool
validChain = Chain.Abs.validChain . absChainFragment

validChainFragment :: ChainFragment -> Bool
validChainFragment = Chain.Abs.validChainFragment . absChainFragment

genChain :: Int -> Gen Chain
genChain n = reifyChainFragment <$> Chain.Abs.genChain n

newtype TestChain = TestChain Chain
    deriving Show

instance Arbitrary TestChain where
    arbitrary = do
        Positive n <- arbitrary
        TestChain <$> genChain n

prop_TestChain :: TestChain -> Bool
prop_TestChain (TestChain chain) = validChain chain

-- |
-- TODO: like 'Chain.Volatile.invChainState'
invChain :: Chain -> Bool
invChain = undefined
