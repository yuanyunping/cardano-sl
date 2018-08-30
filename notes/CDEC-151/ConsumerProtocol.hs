{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
module ConsumerProtocol where

import           Prelude

-- import           Data.Word
import           Control.Applicative
-- import           Control.Concurrent.STM (STM, retry)
-- import           Control.Exception (assert)
import           Control.Monad
-- import           Control.Monad.ST.Lazy
import           Control.Monad.Free (Free (..))
import           Control.Monad.Free as Free
import           Control.Monad.ST.Lazy (runST)
import           Data.FingerTree (ViewL (..))
import           Data.FingerTree as FT
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
-- import           Data.STRef.Lazy
import           System.Random (mkStdGen)

import           Test.QuickCheck

import           Block (Block (..), Point, ReaderId, blockPoint)
import           Chain (Chain, ChainFragment (..), absChainFragment, applyChainUpdate, chain,
                        chainGenesis, findIntersection)
import qualified Chain as Chain
import           Chain.Update (ChainUpdate (..))
import           ChainExperiment2
import           MonadClass
import           Sim (ProbeTrace, SimChan (..), SimF, SimM, SimMVar (..), SimProbe, Trace, failSim,
                      flipSimChan, newProbe, readProbe, runSimM, runSimMST)
import qualified Sim as Sim

--
-- IPC based protocol
--

-- | In this protocol the consumer always initiates things and the producer
-- replies. This is the type of messages that the consumer sends.
data MsgConsumer = MsgRequestNext
                 | MsgSetHead Point [Point]
    deriving (Show)

-- | This is the type of messages that the producer sends.
data MsgProducer = MsgRollForward  Block
                 | MsgRollBackward Point
                 | MsgAwaitReply
                 | MsgIntersectImproved Point Point
                 | MsgIntersectUnchanged
    deriving (Show)

data ConsumerHandlers m = ConsumerHandlers {
       getChainPoints :: m (Point, [Point]),
       addBlock       :: Block -> m (),
       rollbackTo     :: Point -> m ()
     }

consumerSideProtocol1 :: forall m.
                         (MonadSendRecv m, MonadSay m)
                      => ConsumerHandlers m
                      -> Int  -- ^ consumer id
                      -> Bool -- ^ finish on `MsgAwaitReply`
                      -> BiChan m MsgConsumer MsgProducer
                      -> m ()
consumerSideProtocol1 ConsumerHandlers{..} n done chan = do
    -- The consumer opens by sending a list of points on their chain.
    -- This includes the head block and
    (hpoint, points) <- getChainPoints
    sendMsg chan (MsgSetHead hpoint points)
    MsgIntersectImproved{} <- recvMsg chan
    requestNext
  where
    consumerId :: String
    consumerId = "consumer-" ++ show n

    requestNext :: m ()
    requestNext = do
      sendMsg chan MsgRequestNext
      reply <- recvMsg chan
      k <- handleChainUpdate reply
      when k
        requestNext

    handleChainUpdate :: MsgProducer -> m Bool
    handleChainUpdate msg@MsgAwaitReply = do
      say (consumerId ++ ":handleChainUpdate: " ++ show msg)
      return (not done)

    handleChainUpdate msg@(MsgRollForward  b) = do
      say (consumerId ++ ":handleChainUpdate: " ++ show msg)
      addBlock b
      return True

    handleChainUpdate msg@(MsgRollBackward p) = do
      say (consumerId ++ ":handleChainUpdate: " ++ show msg)
      rollbackTo p
      return True

    handleChainUpdate msg = do
        say (consumerId ++ ":handleChainUpdate: " ++ show msg)
        return True

exampleConsumer :: forall m. (MonadSay m, MonadConc m)
                => MVar m (ChainFragment, MVar m ChainFragment)
                -> ConsumerHandlers m
exampleConsumer chainvar = ConsumerHandlers {..}
    where
    getChainPoints :: m (Point, [Point])
    getChainPoints = do
        (chain, _) <- readMVar chainvar
        -- TODO: bootstraping case (client has no blocks)
        let (p : ps) = map blockPoint $ absChainFragment chain
        return (p, ps)

    addBlock :: Block -> m ()
    addBlock b = void $ modifyMVar_ chainvar $ \(chain, mchain) -> do
        say $ "addBlock: " ++ show b
        let !chain' = applyChainUpdate (AddBlock b) chain
        -- wake up awaiting producer
        _ <- tryPutMVar mchain chain'
        mchain' <- newEmptyNamedMVar (Just $ "c-chainvar-" ++ show (Chain.length chain'))
        return (chain', mchain')

    rollbackTo :: Point -> m ()
    rollbackTo p = void $ modifyMVar_ chainvar $ \(chain, mchain) -> do
        say $ "rollbackTo: " ++ show p
        let !chain' = applyChainUpdate (RollBack p) chain
        -- wake up awaiting producer
        _ <- tryPutMVar mchain chain'
        mchain' <- newEmptyNamedMVar (Just $ "c-chainvar-" ++ show (Chain.length chain'))
        return (chain', mchain')

data ProducerHandlers m r = ProducerHandlers {
       findIntersectionRange :: Point -> [Point] -> m (Maybe (Point, Point)),
       establishReaderState  :: Point -> Point -> m r,
       updateReaderState     :: r -> Point -> Maybe Point -> m (),
       tryReadChainUpdate    :: r -> m (Maybe (ConsumeChain Block)),
       readChainUpdate       :: r -> m (ConsumeChain Block)
     }

-- |
-- TODO:
--  * n-consumers to producer (currently 1-consumer to producer)
producerSideProtocol1 :: forall m r.
                         (MonadSendRecv m, MonadSay m)
                      => ProducerHandlers m r
                      -> Int -- producer id
                      -> BiChan m MsgProducer MsgConsumer
                      -> m ()
producerSideProtocol1 ProducerHandlers{..} n chan =
    awaitOpening >>= awaitOngoing
  where
    producerId :: String
    producerId = "producer-" ++ show n

    awaitOpening = do
      -- The opening message must be this one, to establish the reader state
      say (producerId ++ ":awaitOpening")
      msg@(MsgSetHead hpoint points) <- recvMsg chan
      say $ producerId ++ ":awaitOpening:recvMsg: " ++ show msg
      intersection <- findIntersectionRange hpoint points
      case intersection of
        Just (pt, pt') -> do
          r <- establishReaderState hpoint pt
          let msg = MsgIntersectImproved pt pt'
          say $ producerId ++ ":awaitOpening:sendMsg: " ++ show msg
          sendMsg chan (MsgIntersectImproved pt pt')
          return r
        Nothing -> do
          say $ producerId ++ ":awaitOpening:sendMsg: " ++ show MsgIntersectUnchanged
          sendMsg chan MsgIntersectUnchanged
          awaitOpening

    awaitOngoing r = forever $ do
      msg <- recvMsg chan
      say $ producerId ++ ":awaitOngoing:recvMsg: " ++ show msg
      case msg of
        MsgRequestNext           -> handleNext r
        MsgSetHead hpoint points -> handleSetHead r hpoint points

    handleNext r = do
      mupdate <- tryReadChainUpdate r
      update  <- case mupdate of
        Just update -> return update

        -- Reader is at the head, have to wait for producer state changes.
        Nothing -> do
          say $ producerId ++ ":handleNext:sendMsg: " ++ show MsgAwaitReply
          sendMsg chan MsgAwaitReply
          readChainUpdate r
      let msg = updateMsg update
      say $ producerId ++ ":handleNext:sendMsg: " ++ show msg
      sendMsg chan msg

    handleSetHead r hpoint points = do
      -- TODO: guard number of points, points sorted
      -- Find the most recent point that is on our chain, and the subsequent
      -- point which is not.
      intersection <- findIntersectionRange hpoint points
      case intersection of
        Just (pt, pt') -> do
          updateReaderState r hpoint (Just pt)
          let msg = MsgIntersectImproved pt pt'
          say $ producerId ++ ":handleSetHead:sendMsg: " ++ show msg
          sendMsg chan msg
        Nothing -> do
          updateReaderState r hpoint Nothing
          let msg = MsgIntersectUnchanged
          say $ producerId ++ ":handleSetHead:sendMsg: " ++ show msg
          sendMsg chan msg

    updateMsg (RollForward  b) = MsgRollForward b
    updateMsg (RollBackward p) = MsgRollBackward p


exampleProducer :: forall m. (MonadConc m, MonadSay m)
                => MVar m (ChainProducerState ChainFragment, MVar m (ChainProducerState ChainFragment))
                -> ProducerHandlers m ReaderId
exampleProducer chainvar =
    ProducerHandlers {..}
  where
    findIntersectionRange :: Point -> [Point] -> m (Maybe (Point, Point))
    findIntersectionRange hpoint points = do
      (ChainProducerState {chainState}, _) <- readMVar chainvar
      return $! findIntersection chainState hpoint points

    establishReaderState :: Point -> Point -> m ReaderId
    establishReaderState hpoint ipoint =
      modifyMVar chainvar $ \(cps, mcps) ->
          case initialiseReader hpoint ipoint cps of
            (cps', rid) -> return ((cps', mcps), rid)

    updateReaderState :: ReaderId -> Point -> Maybe Point -> m ()
    updateReaderState rid hpoint mipoint =
      modifyMVar_ chainvar $ \(cps, mcps) ->
        let !ncps = updateReader rid hpoint mipoint cps
        in return (ncps, mcps)

    tryReadChainUpdate :: ReaderId -> m (Maybe (ConsumeChain Block))
    tryReadChainUpdate rid =
      modifyMVar chainvar $ \cps -> do
        let res = readerInstruction (fst cps) rid
        say ("tryReadChainUpdate: ChainProducerState  " ++ show (fst cps))
        say ("tryReadChainUpdate: ChainProducerState' " ++ show (fmap fst res))
        say ("tryReadChainUpdate: instruction         " ++ show (fmap snd res))
        return $ swizzle cps res
      where
        swizzle cps       Nothing          = (cps, Nothing)
        swizzle (_, mcps) (Just (cps', x)) = ((cps', mcps), Just x)

    readChainUpdate :: ReaderId -> m (ConsumeChain Block)
    readChainUpdate rid = do
      -- block on the inner mvar
      say ("readChainUpdate: blocking on updated for " ++ show rid)
      cps <- readMVar chainvar >>= readMVar . snd
      say ("readChainUpdate: unblocked on update for " ++ show rid)
      say ("readChainUpdate: ChainProducerState " ++ show cps)
      case readerInstruction cps rid of
          Just (_, x) -> do
            return x
          -- NOTE: this will block until we know how to update the
          -- consumer; it may never end. Maybe we should just fail.
          Nothing     -> readChainUpdate rid


-- |
-- Simulate transfering a chain from a producer to a consumer
producerToConsumerSim
    :: SimProbe s Chain
    -> Chain
    -- ^ chain to reproduce on the consumer; ought to be non empty
    -> Chain
    -- ^ initial chain of the consumer; ought to be non empty
    -> Free (SimF s) ()
producerToConsumerSim v chain cchain = do
    chan <- newChan

    -- run producer in a new thread
    fork $ do
        mcps     <- newEmptyMVar
        chainvar <- newMVar (ChainProducerState chain [], mcps)
        producerSideProtocol1 (exampleProducer chainvar) 1 chan

    mchain <- newEmptyMVar
    chainvar <- newMVar (cchain, mchain)

    fork $ do
        consumerSideProtocol1 (exampleConsumer chainvar) 1 False (flipSimChan chan)

    fork $ forever $ do
      (chain, mchain) <- readMVar chainvar
      void $ probeOutput v chain
      void $ readMVar mchain

runProducerToConsumer :: Chain -> Chain -> (Trace, ProbeTrace Chain)
runProducerToConsumer pchain cchain = runST $ do
  v <- newProbe
  trace <- runSimMST (mkStdGen 0) (producerToConsumerSim v pchain cchain)
  probe <- readProbe v
  return (trace, probe)

prop_producerToConsumer :: Chain.ChainFork -> Property
prop_producerToConsumer (Chain.ChainFork pchain cchain) =
  let (tr, pr) = runProducerToConsumer pchain cchain
      rchain = snd $ last pr -- ^ chain transferred to the consumer
  in counterexample
      ("producer chain: " ++ show pchain ++
       "\nconsumer chain: " ++ show cchain ++
       "\nresult chain: " ++ show rchain ++
       "\ntrace:\n" ++ unlines (map show $ filter Sim.filterTrace tr)
       )
    $ rchain == pchain

-- |
-- For a producer that is following n consumers (is subscribed to n-nodes)
-- we will need `producerSideProtocol1` that is aware of all its subscriptions
-- and has access to all channels.
bindConsumersToProducerN
    :: ChainFragment
    -- ^ initial producer's chain
    -> (ChainFragment -> ChainFragment -> ChainFragment)
    -- ^ pure chain selection
    -> [ SimMVar s
          ( ChainFragment
          , SimMVar s ChainFragment
          )
       ]
       -- ^ list of consumer's mvars
    -> SimM s
        (SimMVar s
          ( ChainProducerState ChainFragment
          , SimMVar s (ChainProducerState ChainFragment)
          ))
bindConsumersToProducerN chain selectChain ts = do
  chainvar <- newEmptyMVar >>= newMVar . (ChainProducerState chain [],)
  fork (go chainvar (zip [1..] ts))
  return chainvar
  where

  -- fork a thread for each listener
  go chainvar [] = return ()
  go chainvar ((i, m) : ms) = do
    fork $ listen i m chainvar
    go chainvar ms

  listen
    :: Int
    -> SimMVar s
      ( ChainFragment
      , SimMVar s ChainFragment
      ) -- ^ source
    -> SimMVar s
      ( ChainProducerState ChainFragment
      , SimMVar s (ChainProducerState ChainFragment)
      ) -- ^ target
    -> SimM s ()
  listen i src tgt = forever $ readMVar src >>= \(src_chain, msrc_chain) -> do
    modifyMVar_ tgt $ \(cps@(ChainProducerState tgt_chain _), mcps) -> do
      let !cps' = cps { chainState = selectChain tgt_chain src_chain }
      x <- tryPutMVar mcps cps'
      say $ "bind:listen:tryPutMVar: (" ++ show i ++ "): " ++ show x
      say $ "bind:listen:tryPutMVar: (" ++ show i ++ "): " ++ show (chainReaders cps')
      say $ "bind:listen:modifyMVar: (" ++ show i ++ "): " ++ show (chainState cps')
      mcps' <- newEmptyMVar
      return (cps', mcps')
    -- block until new changes
    void $ readMVar msrc_chain

-- |
-- Simulate a node which is subscribed to two producers and has a single
-- subscription
--
--   producer1      producer2
--      |               |
--      V               V
-- ----------------------------
-- | listener1      listener2 |
-- |       \          /       |
-- |        \        /        |
-- |         \      /         |
-- |          \    /          |
-- |         producer         |
-- ----------------------------
--               |
--               v
--           listener
nodeSim
    :: SimProbe s Chain
    -> Chain
    -- ^ initial chain of producer 1
    -> Chain
    -- ^ initial chain of producer 2
    -> Free (SimF s) ()
nodeSim v chain1 chain2 = do
    chan1 <- newChan -- producer1 to listener1
    chan2 <- newChan -- producer2 to listener2
    chan3 <- newChan -- producer  to listener

    -- start producer1
    fork $ do
        mcps     <- newEmptyMVar
        chainvar <- newMVar (ChainProducerState chain1 [], mcps)
        producerSideProtocol1 (exampleProducer chainvar) 1 chan1

    -- start producer2
    fork $ do
        mcps     <- newEmptyMVar
        chainvar <- newMVar (ChainProducerState chain2 [], mcps)
        producerSideProtocol1 (exampleProducer chainvar) 2 chan2

    -- consumer listening to producer1
    let genesisBlock1 = case chainGenesis chain1 of
            Nothing -> error "sim2: chain1 is empty"
            Just b  -> b
    mchain1   <- newEmptyMVar
    chainvar1 <- newMVar (Chain.singleton genesisBlock1, mchain1)
    fork $
        consumerSideProtocol1 (exampleConsumer chainvar1) 1 False (flipSimChan chan1)

    -- consumer listening to producer2
    let genesisBlock2 = case chainGenesis chain2 of
            Nothing -> error "sim2: chain2 is empty"
            Just b  -> b
    mchain2   <- newEmptyMVar
    chainvar2 <- newMVar (Chain.singleton genesisBlock2 , mchain2)
    fork $
        consumerSideProtocol1 (exampleConsumer chainvar2) 2 False (flipSimChan chan2)

    fork $ do
        chainvar <- bindConsumersToProducerN
            (Chain.singleton genesisBlock1)
            Chain.selectChain
            [chainvar1, chainvar2]
        producerSideProtocol1 (exampleProducer chainvar) 3 chan3

    mchain    <- newEmptyMVar
    chainvar3 <- newNamedMVar (Just "chainvar-3")
      ( (Chain.fromList [genesisBlock1, Block 5 1 2 ""])
      , mchain
      )
    fork
      $ consumerSideProtocol1 (exampleConsumer chainvar3) 3 False (flipSimChan chan3)

    fork $ forever $ do
      (chain, mchain) <- readMVar chainvar3
      void $ probeOutput v chain
      void $ readMVar mchain

runNodeSim :: Chain -> Chain -> (Trace, ProbeTrace Chain)
runNodeSim pchain1 pchain2 = runST $ do
  v <- newProbe
  trace <- runSimMST (mkStdGen 0) (nodeSim v pchain1 pchain2)
  probe <- readProbe v
  return (trace, probe)

prop_node :: Chain.ChainFork -> Property
prop_node (Chain.ChainFork pchain1 pchain2) =
  let (tr, pr) = runNodeSim pchain1 pchain2
      rchain = snd $ last pr
      schain = Chain.selectChain pchain1 pchain2
  in counterexample
      ("producer chain1: " ++ show pchain1 ++
       "\nprocuder chain2: " ++ show pchain2 ++
       "\nresult chain: " ++ show rchain ++
       "\nselect chain: " ++ show schain ++
       "\ntrace:\n" ++ unlines (map show $ filter Sim.filterTrace tr)
       )
    $ rchain == schain

-- This case fails!
-- Block { blockId = 2 } ends up twice in the chain:
-- * readChainUpdate sends it
-- * immediatelly after tryReadChain sends it too
-- The reader state has not progressed between the calls
prop_node1 :: Property
prop_node1 = prop_node $ Chain.ChainFork
    (ChainFragment (FT.fromList [Block {blockId = 1, prevBlockId = 0, blockSlot = 1, blockPayload = "VYMB"}]))
    (ChainFragment (FT.fromList [Block {blockId = 1, prevBlockId = 0, blockSlot = 1, blockPayload = "VYMB"},Block {blockId = 2, prevBlockId = 1, blockSlot = 2, blockPayload = "FEWU"},Block {blockId = 3, prevBlockId = 2, blockSlot = 3, blockPayload = "ECJK"}]))
--
-- Simulation of composition of producer and consumer
--


-- | Given two sides of a protocol, ...
--
simulateWire
  :: forall p c s .
     (SimChan s p c -> Free (SimF s) ())
  -> (SimChan s c p -> Free (SimF s) ())
  -> Free (SimF s) ()
simulateWire protocolSideA protocolSideB = do
    chan <- newChan
    fork $ protocolSideA chan
    fork $ protocolSideB (flipSimChan chan)
    return ()

return []
runTests = $quickCheckAll
