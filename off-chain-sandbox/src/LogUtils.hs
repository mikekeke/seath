module LogUtils where

import Data.Set (member)
import Data.Text (pack, unpack)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)

data LogType
  = Send
  | Receive
  | Switch
  | DiscardLoop
  | BeforeReset
  | HashUpdate
  deriving stock (Eq, Ord, Show)

log :: Set LogType -> LogType -> Text -> IO ()
log allowed current msg = do
  when (current `member` allowed) (printMsg msg)
  where
    appendType s = show current <> ": " <> s
    printMsg = withTime (putStrLn . unpack) . appendType

withTime :: (Text -> IO ()) -> (Text -> IO ())
withTime act = \msg -> do
  getCurrentTime >>= act . addTime msg
  where
    addTime msg ct = ("[" <> pack (iso8601Show ct) <> "]: ") <> msg

dSet :: Set LogType
dSet =
  fromList
    [ Switch,
      BeforeReset 
      --,
      -- Send,
      -- Receive,
      -- HashUpdate
    ]

dLog :: LogType -> Text -> IO ()
dLog = log dSet
