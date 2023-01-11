module Main where

import Data.Text qualified as T
import HttpNode (startHttpNode)
import Network (mkSlotTracker)
import Text.Read (read)

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering

  slotTracker <- mkSlotTracker

  -- start some nodes for debug
  let debugNodes = [3000, 3001, 3002, 3003, 3004, 3005, 3006, 3007]
      run = runCmd slotTracker
  putStrLn $ "Auto-starting debug nodes on ports " <> show debugNodes
  mapM_ (run . ("sn " <>) . show) debugNodes

  putStrLn "Enter command:"
  forever $ do
    cmd <- getLine
    run cmd
  where
    runCmd st cmd =
      case words cmd of
        ["sn", p] -> startHttpNode st (readT p)
        -- ["sdn"] -> startDebugNode ticker
        _ -> die "hard"

readT :: Read a => Text -> a
readT = read . T.unpack
