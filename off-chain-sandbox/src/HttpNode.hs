module HttpNode (startHttpNode) where

import Control.Concurrent (ThreadId, forkIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Data.UUID.V4 (nextRandom)
import Network.HTTP.Simple
  ( Response,
    httpNoBody,
    parseRequest_,
    setRequestBodyJSON,
  )
import Node (
    debugParent,
    debugPeers,
    listenNode,
    startNode,
    tellNode',
  )
import Types
import Web.Scotty qualified as S

startHttpNode :: NodeAddr -> IO ThreadId
startHttpNode port = forkIO $ do
  node <- startNode port -- >>= addPeer 8888
  listenNode node outMsgHandler

  let tellN = tellNode' node
  print $ "HttpNode started: " <> show node
  S.scotty port $ do
    S.post "/add-tx" $ do
      uid <- UUID.toText <$> liftIO nextRandom
      tellN (AddTx uid)
      S.json @Text "Ok"

    S.post "/take-peers" $ do
      receivedPeers :: [NodeAddr] <- S.jsonData
      putStrLn $ "Node #" <> show (nodeAddr node) <> " got peers: " <> show receivedPeers

    S.post "/connect" $ do
      toPort :: NodeAddr <- S.jsonData
      tellN (InitConnection toPort)

    S.post "/take-parent" $ do
      candidate :: RootInfo <- S.jsonData
      tellNode' node (ReceiveParent candidate)

    S.get "/d-get-peers" $ do
      ps <- liftIO $ debugPeers node
      S.json ps

    S.get "/d-get-parent" $ do
      ps <- liftIO $ debugParent node
      S.json ps

    S.post "/incoming-connect" $ do
      fromAddr :: NodeAddr <- S.jsonData
      tellNode' node (IncomingPeer fromAddr)

    S.post "/accepted" $ do
      accepted :: NodeAddr <- S.jsonData
      tellNode' node (PeerAccepted accepted)
      tellNode' node AnnounceRoot

outMsgHandler :: OutMessage -> IO ()
outMsgHandler = \case
  MyParent ps parent -> forM_ ps (\p -> postTo parent p "/take-parent")
  RequestConnection from to -> void $ postTo from to "/incoming-connect"
  PeerIsOk me requester -> void $ postTo me requester "/accepted"
  where
    postTo :: (ToJSON a) => a -> NodeAddr -> Text -> IO (Response ()) -- FIXME: something better than Text?
    postTo payload to method = do
      httpNoBody
        . setRequestBodyJSON payload
        . parseRequest_
        . T.unpack
        $ "POST http://localhost:" <> show to <> method

instance ToJSON RootInfo
instance FromJSON RootInfo