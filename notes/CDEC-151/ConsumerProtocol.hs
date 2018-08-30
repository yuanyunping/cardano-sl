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
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
module ConsumerProtocol where

-- import           Data.Word
import           Control.Applicative
-- import           Control.Concurrent.STM (STM, retry)
-- import           Control.Exception (assert)
import           Control.Monad
-- import           Control.Monad.ST.Lazy
import           Control.Monad.Free (Free (..))
import           Control.Monad.Free as Free
import           Data.FingerTree (ViewL (..))
import           Data.FingerTree as FT
-- import           Data.STRef.Lazy
import           System.Random (mkStdGen)

-- import           Test.QuickCheck

import           Block (Block (..), Point, ReaderId, blockPoint)
import           Chain (Chain, ChainFragment (..), absChainFragment, applyChainUpdate, chain,
                        findIntersection)
import           Chain.Update (ChainUpdate (..))
import           ChainExperiment2
import           MonadClass
import           Sim (SimChan (..), SimF, SimM, SimMVar (..), Trace, failSim, flipSimChan, runSimM)

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
                      -> Bool -- ^ finish on `MsgAwaitReply`
                      -> BiChan m MsgConsumer MsgProducer
                      -> m ()
consumerSideProtocol1 ConsumerHandlers{..} done chan = do
    -- The consumer opens by sending a list of points on their chain.
    -- This includes the head block and
    (hpoint, points) <- getChainPoints
    sendMsg chan (MsgSetHead hpoint points)
    MsgIntersectImproved{} <- recvMsg chan
    requestNext
  where
    requestNext :: m ()
    requestNext = do
      sendMsg chan MsgRequestNext
      reply <- recvMsg chan
      k <- handleChainUpdate reply
      when k
        requestNext

    handleChainUpdate :: MsgProducer -> m Bool
    handleChainUpdate msg@MsgAwaitReply = do
      say ("consumer: " ++ show msg)
      return (not done)

    handleChainUpdate msg@(MsgRollForward  b) = do
      say ("consumer: " ++ show msg)
      addBlock b
      return True

    handleChainUpdate msg@(MsgRollBackward p) = do
      say ("consumer: " ++ show msg)
      rollbackTo p
      return True

exampleConsumer :: forall m. MonadConc m
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
        let !chain' = applyChainUpdate (AddBlock b) chain
        -- wake up awaiting producer
        _ <- tryPutMVar mchain chain'
        mchain' <- newEmptyMVar
        return (chain', mchain')

    rollbackTo :: Point -> m ()
    rollbackTo p = void $ modifyMVar_ chainvar $ \(chain, mchain) -> do
        let !chain' = applyChainUpdate (RollBack p) chain
        -- wake up awaiting producer
        _ <- tryPutMVar mchain chain'
        mchain' <- newEmptyMVar
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
                      -> BiChan m MsgProducer MsgConsumer
                      -> m ()
producerSideProtocol1 ProducerHandlers{..} chan =
    awaitOpening >>= awaitOngoing
  where
    awaitOpening = do
      -- The opening message must be this one, to establish the reader state
      say "producer:awaitOpening"
      msg@(MsgSetHead hpoint points) <- recvMsg chan
      say $ "producer:awaitOpening:recvMsg: " ++ show msg
      intersection <- findIntersectionRange hpoint points
      case intersection of
        Just (pt, pt') -> do
          r <- establishReaderState hpoint pt
          let msg = MsgIntersectImproved pt pt'
          say $ "producer:awaitOpening:sendMsg: " ++ show msg
          sendMsg chan (MsgIntersectImproved pt pt')
          return r
        Nothing -> do
          say $ "producer:awaitOpening:sendMsg: " ++ show MsgIntersectUnchanged
          sendMsg chan MsgIntersectUnchanged
          awaitOpening

    awaitOngoing r = forever $ do
      msg <- recvMsg chan
      say $ "producer:awaitOngoing:recvMsg: " ++ show msg
      case msg of
        MsgRequestNext           -> handleNext r
        MsgSetHead hpoint points -> handleSetHead r hpoint points

    handleNext r = do
      mupdate <- tryReadChainUpdate r
      update  <- case mupdate of
        Just update -> return update

        -- Reader is at the head, have to wait for producer state changes.
        Nothing -> do
          say $ "producer:handleNext:sendMsg: " ++ show MsgAwaitReply
          sendMsg chan MsgAwaitReply
          readChainUpdate r
      let msg = updateMsg update
      say $ "producer:handleNext:sendMsg: " ++ show msg
      sendMsg chan (updateMsg update)

    handleSetHead r hpoint points = do
      -- TODO: guard number of points, points sorted
      -- Find the most recent point that is on our chain, and the subsequent
      -- point which is not.
      intersection <- findIntersectionRange hpoint points
      case intersection of
        Just (pt, pt') -> do
          updateReaderState r hpoint (Just pt)
          let msg = MsgIntersectImproved pt pt'
          say $ "producer:handleSetHead:sendMsg: " ++ show msg
          sendMsg chan msg
        Nothing -> do
          updateReaderState r hpoint Nothing
          let msg = MsgIntersectUnchanged
          say $ "producer:handleSetHead:sendMsg: " ++ show msg
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
        say ("tryReadChainUpdate: " ++ show (fmap snd res))
        return $ (swizzle cps res)
      where
        swizzle cps       Nothing          = (cps, Nothing)
        swizzle (_, mcps) (Just (cps', x)) = ((cps', mcps), Just x)

    readChainUpdate :: ReaderId -> m (ConsumeChain Block)
    readChainUpdate rid = do
      -- block on the inner mvar
      say ("readChainUpdate: blocking on updated for " ++ show rid)
      cps <- readMVar chainvar >>= readMVar . snd
      case readerInstruction cps rid of
          Just (_, x) -> return x
          -- NOTE: this will block until we know how to update the
          -- consumer; it may never end. Maybe we should just fail.
          Nothing     -> readChainUpdate rid

-- |
-- For a producer that is following n consumers (is subscribed to n-nodes)
-- we will need `producerSideProtocol1` that is aware of all its subscriptions
-- and has access to all channels.
bindConsumersToProducer1
    :: SimMVar s
            ( ChainProducerState ChainFragment
            , SimMVar s (ChainProducerState ChainFragment)
            )
    -> SimMVar s
        ( ChainProducerState ChainFragment
        , SimMVar s (ChainProducerState ChainFragment)
        )
bindConsumersToProducer1 = id

-- |
-- Simulate transfering a chain from a producer on a node-1 to a consumer on
-- a node-2.
sim1
    :: Chain
    -- ^ chain to reproduce on the consumer, ought to be non empty
    -> SimM s ()
sim1 chain@(ChainFragment ft) = do
    chan <- newChan

    -- run producer in a new thread
    fork $ do
        mcps <- newEmptyMVar
        chainvar  <- newMVar (ChainProducerState chain [], mcps)
        producerSideProtocol1 (exampleProducer chainvar) chan

    -- run consumer
    let genesisBlock = case viewl ft of
            EmptyL -> error "sim1: chain is empty"
            b :< _ -> b
    mchain <- newEmptyMVar
    chainvar <- newMVar
      ( (ChainFragment $ FT.fromList [genesisBlock, Block 5 1 2 ""])
      , mchain
      )
    consumerSideProtocol1 (exampleConsumer chainvar) True (flipSimChan chan)

    (chain', _) <- readMVar chainvar
    when (chain /= chain')
      (failSim $ "chains not equal: " ++ show chain')
    say "Bye!"

trace1 :: Trace
trace1 = runSimM (mkStdGen 0) (sim1 chain)

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
