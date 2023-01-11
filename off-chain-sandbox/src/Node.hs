{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Node
  ( Node,
    NodeAddr,
    -- PeerAddr,
    startNode,
    tellNode,
    killNode,
    nodeAddr,
    tellNode',
    InMessage (..),
    OutMessage (..),
    listenNode,
    nodeParent,
    RootInfo (..),
    debugPeers,
    debugParent,
  )
where

import Control.Concurrent
  ( ThreadId,
    dupChan,
    forkIO,
    killThread,
    modifyMVar_,
    newChan,
    readChan,
    writeChan,
  )
import Data.List.NonEmpty qualified as NE
import Data.Sequence ((|>))
import Data.Set qualified as S
import LogUtils
import Network (eachRound, genNetId, mkSlotTracker)
import Types
  ( InMessage (..),
    NState (NState, nodeParent, peers, transactions),
    Node (..),
    NodeAddr,
    NodeState,
    OutMessage (..),
    -- PeerAddr,
    RootInfo (..),
    Tx,
    getSlot, SlotTracker,
  )

{- TODO

-}

startNode :: SlotTracker IO -> NodeAddr -> IO Node
startNode slotTracker nodeAddr = do
  inChan <- newChan
  outChan <- newChan
  nState <- newMVar (NState mempty mempty Nothing mempty)
  nodeTids <- newMVar []
  nodeNetId <- newMVar ""
  let node = Node {..}
  idTid <- idThread node
  nodeTids' <- handleNodeProcess node >>= newMVar . (idTid :)
  return $ node {nodeTids = nodeTids'}

-- start process of generating and setting Node id each "round"
idThread :: Node -> IO ThreadId
idThread node = forkIO $ do
  assignId
  eachRound assignId
  where
    assignId = do
      nId <- genNetId node
      void $ swapMVar (nodeNetId node) nId
      void $ resetParent node
      dLog HashUpdate $ show node <> " -> " <> show nId
      tellNode' node AnnounceRoot

handleNodeProcess :: Node -> IO [ThreadId]
handleNodeProcess node = do
  let ns = nState node
      addr = nodeAddr node
  print (mconcat [show node, ": starts listening inbox"] :: Text)
  t1 <- forkIO $
    forever $ do
      msg <- readChan (inChan node)
      dLog Receive (mconcat [show node, ": ", show msg])
      case msg of
        AddTx tx -> addTx tx ns
        IncomingPeer p -> addPeer p node >> transmit node (PeerIsOk addr p)
        ReceiveParent p -> selectParent node p
        AnnounceRoot -> announceParentToPeers node -- FIXME: better naming and logic
        PeerAccepted p -> addPeer p node
        InitConnection to -> transmit node (RequestConnection addr to)
  return [t1]

listenNode :: Node -> (OutMessage -> IO a) -> IO ()
listenNode n handle = do
  listenChan <- dupChan (outChan n)
  tid <- forkIO $ do
    forever $ do
      out <- readChan listenChan
      dLog Send $ show n <> ": " <> show out
      handle out
  modifyMVar_ (nodeTids n) (pure . (tid :))

killNode :: Node -> IO ()
killNode n = readMVar (nodeTids n) >>= mapM_ killThread

announceParentToPeers :: Node -> IO ()
announceParentToPeers n = do
  peers <- getPeers n
  announceParentTo n peers

announceParentTo :: Node -> Set NodeAddr -> IO ()
announceParentTo n receivers = do
  unless (S.null receivers) $ do
    currSlot <- getSlot (slotTracker n)
    maybeParent <- getParent n
    hashId <- readNetId n
    let announcer = nodeAddr n
        parent = case maybeParent of
          Nothing -> RootInfo (nodeAddr n) currSlot (NE.singleton announcer) hashId
          Just p -> p {announcers = NE.cons announcer (announcers p)}
    transmit n $ MyParent receivers parent

selectParent :: Node -> RootInfo -> IO ()
selectParent node incomingInfo = do
  mParent <- getParent node
  currentInfo <-
    case mParent of
      Nothing -> do
        currSlot <- getSlot (slotTracker node)
        hashId <- readNetId node
        pure $ RootInfo (nodeAddr node) currSlot (NE.singleton $ nodeAddr node) hashId
      Just p -> pure p

  if rootAddr incomingInfo == nodeAddr node
    || sameAnnounce incomingInfo currentInfo
    then do
      dLog DiscardLoop $ show node <> " discarding parent announce loop"
      pure ()
    else do
      if isBetter currentInfo incomingInfo
        then do
          -- reply back to sender with own root
          announceParentTo node (S.singleton $ head $ announcers incomingInfo)
        else do
          setParent node incomingInfo
          peers <- getPeers node
          announceParentTo node peers
  where
    isBetter current new =
      -- (announceTime current >= announceTime new)
      --   -- && (announcer current /= announcer new)
      --   &&
      (parentNodeId current <= parentNodeId new)

debugPeers :: Node -> IO (Set NodeAddr)
debugPeers = getPeers

debugParent :: Node -> IO (Maybe RootInfo)
debugParent = getParent

----------------------------
-- node utils and helpers --
----------------------------
sameAnnounce :: RootInfo -> RootInfo -> Bool
sameAnnounce p1 p2 =
  ((==) `on` announceTime) p1 p2
    && ((==) `on` rootAddr) p1 p2

-- nodeId = nodeAddr

readNetId :: Node -> IO Text
readNetId = readMVar . nodeNetId

nodeNetEqual :: Node -> Node -> Bool
nodeNetEqual n1 n2 =
  ((==) `on` nodeAddr) n1 n2
    && ((==) `on` nodeNetId) n1 n2

getParent :: Node -> IO (Maybe RootInfo)
getParent = fmap nodeParent . readMVar . nState

getPeers :: Node -> IO (Set NodeAddr)
getPeers = fmap peers . readMVar . nState

setParent :: Node -> RootInfo -> IO ()
setParent node p = do
  dLog Switch $ show node <> " to " <> show p
  modifyMVar_
    (nState node)
    (\s -> pure $ s {nodeParent = Just p})

resetParent :: Node -> IO ()
resetParent node = do
  modifyMVar_
    (nState node)
    ( \s -> do
        dLog BeforeReset $ show node <> ": " <> show (nodeParent s)
        pure $ s {nodeParent = Nothing}
    )

addTx :: Tx -> NodeState -> IO ()
addTx tx ns = do
  modifyMVar_
    ns
    (\s -> pure $ s {transactions = transactions s |> tx})

addPeer :: NodeAddr -> Node -> IO ()
addPeer p n = do
  modifyMVar_
    (nState n)
    (\s -> pure $ s {peers = S.insert p (peers s)})

tellNode :: Node -> InMessage -> IO ()
tellNode n = writeChan (inChan n)

tellNode' :: MonadIO m => Node -> InMessage -> m ()
tellNode' = (liftIO .) . tellNode

transmit :: MonadIO m => Node -> OutMessage -> m ()
transmit node msg = liftIO $ writeChan (outChan node) msg
