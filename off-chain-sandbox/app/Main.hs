module Main where

import Data.Text qualified as T
import HttpNode (startHttpNode)
import Text.Read (read)

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering

  -- start some nodes for debug
  let debugNodes = [3000, 3001, 3002, 3003, 3004, 3005, 3006, 3007]
  putStrLn $ "Auto-starting debug nodes on ports " <> show debugNodes
  mapM_ (runCmd . ("sn " <>) . show) debugNodes

  putStrLn "Enter command:"
  forever $ do
    cmd <- getLine
    runCmd cmd
  where
    runCmd cmd =
      case words cmd of
        ["sn", p] -> startHttpNode (readT p)
        -- ["sdn"] -> startDebugNode ticker
        _ -> die "hard"

readT :: Read a => Text -> a
readT = read . T.unpack
