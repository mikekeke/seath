module Types where

import Control.Concurrent
  ( Chan,
    ThreadId,
  )
import Text.Show qualified


-- MESSAGES
data InMessage
  = AddTx Tx
  | ReceiveParent RootInfo
  | AnnounceRoot
  | InitConnection NodeAddr
  | IncomingPeer NodeAddr
  | PeerAccepted NodeAddr -- by whom
  -- ChainOf NodeAddr Text
  deriving stock (Show)

data OutMessage
  = MyParent (Set NodeAddr) RootInfo
  | PeerIsOk NodeAddr NodeAddr -- this node id, peer id
  | RequestConnection NodeAddr NodeAddr
  deriving stock (Show)

-- NODE
data Node = Node
  { nodeAddr :: NodeAddr,
    inChan :: Chan InMessage,
    outChan :: Chan OutMessage,
    nState :: NodeState,
    nodeTids :: MVar [ThreadId],
    slotTracker :: SlotTracker IO,
    nodeNetId :: MVar NetId
  }

instance Show Node where
  show n = "Node #" <> show (nodeAddr n)

newtype SlotTracker m = SlotTracker {getSlot :: m Int}

type NetId = Text

type NodeAddr = Int

type NodeState = MVar NState

data NState = NState
  { announces :: ParentAnnounces, -- ? TODO: maybe they should go to HttpNode?
    transactions :: !(Seq Tx),
    nodeParent :: Maybe RootInfo,
    peers :: Set NodeAddr
  }

data RootInfo = RootInfo
  { rootAddr :: NodeAddr,
    announceTime :: Int,
    announcers :: NonEmpty NodeAddr,
    parentNodeId :: Text
  }
  deriving stock (Generic)

instance Show RootInfo where
  show (RootInfo root time anns _) =
    "RootInfo {root = " <> show root
      <> ", parent = "
      <> show (head anns)
      <> ", time = "
      <> show time
      <> "}"

type ParentAnnounces = Map NodeAddr RootInfo

type Tx = Text