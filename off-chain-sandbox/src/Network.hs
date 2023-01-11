module Network (genNetId, mkSlotTracker, eachRound) where

import Control.Concurrent (forkIO, modifyMVar_, threadDelay)
import Crypto.Hash.SHA256 (hash)
import Data.ByteString.Base16 (encode)
import Data.Text.Encoding (decodeUtf8Lenient)
import Types (Node (nodeAddr, slotTracker), SlotTracker (SlotTracker, getSlot))

genNetId :: Node -> IO Text
genNetId node = do
  currSlot <- getSlot (slotTracker node)
  let !nId = decodeUtf8Lenient $ encode $ hash (show (nodeAddr node) <> show currSlot)
  pure nId

eachRound :: IO a -> IO b
eachRound a = forever $ waitSlots roundLen >> a

mkSlotTracker :: IO (SlotTracker IO)
mkSlotTracker = do
  t <- newMVar 0
  _ <- forkIO $ forever $ modifyMVar_ t (pure . succ) >> waitSlots slotSize
  return (SlotTracker (readMVar t))

-------------------
-- slot settings --
-------------------
slotSize :: Int
slotSize = 1

roundLen :: Int
roundLen = slots 10

-------------------
--      utils    --
-------------------
slots :: Int -> Int
slots x = x * slotSize

waitSlots :: Int -> IO ()
waitSlots = threadDelay . (* 1_000_000)
