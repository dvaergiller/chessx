module ChessX.SSE where

import Control.Concurrent.Chan
import Control.Monad.IO.Class
import qualified Data.Map as M
import Network.Wai.EventSource
import Network.Wai
import Servant

import ChessX.Board

type EventChannel = Chan ServerEvent
type ChannelsMap = M.Map BoardId EventChannel

mkEventChannel :: IO EventChannel
mkEventChannel = newChan

notifyGameUpdated :: MonadIO m => BoardId -> ChannelsMap -> m ()
notifyGameUpdated bId channels =
  case M.lookup bId channels of
    Nothing ->
      return ()
    Just ch ->
      liftIO $ writeChan ch (ServerEvent (Just "gameUpdated") Nothing ["update board"])

getChannel :: MonadIO m
           => BoardId
           -> ChannelsMap
           -> m (ChannelsMap, EventChannel)
getChannel bId channels =
  case M.lookup bId channels of
    Nothing -> do
      chan <- liftIO newChan
      return (M.insert bId chan channels, chan)
    Just existing -> do
      chan <- liftIO $ dupChan existing
      return (channels, chan)
      
sseApplication :: EventChannel -> Application
sseApplication =
  eventSourceAppChan
