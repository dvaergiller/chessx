{-# LANGUAGE DataKinds #-}
module ChessX.Server where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import Data.List
import Data.Maybe (isNothing)
import Data.Text (Text, pack)
import Network.Wai (Middleware, Request(..))
import Servant

import ChessX.API
import ChessX.Board
import ChessX.Error
import qualified ChessX.Game as Game
import ChessX.HTMX.Index
import ChessX.HTMX.BoardPage

type ChessXServer = Game.GameT (ErrorT Handler)

application :: IO Application
application = do
  stateVar <- Game.initStateVar
  return $ serve api
         $ hoistServer api (toHandler stateVar) server

toHandler :: Game.StateVar -> ChessXServer a -> Handler a
toHandler stateVar =
  toServerError . Game.runGame stateVar

api :: Proxy API
api = Proxy

server :: ServerT API ChessXServer
server = pagesServer
    :<|> boardServer
    :<|> serveDirectoryFileServer "public"

-- Endpoint handlers:

pagesServer :: ServerT PagesAPI ChessXServer
pagesServer = return Index
         :<|> \bId -> return (BoardPage bId)

boardServer :: ServerT BoardAPI ChessXServer
boardServer = makeNewBoard
         :<|> joinBoard
         :<|> getBoard
         :<|> selectPiece
         :<|> movePiece

makeNewBoard :: CreateBoardRequest
             -> ChessXServer NoContent
makeNewBoard (CreateBoardRequest name team) = do
  (board, token) <- Game.createBoard >>= Game.joinBoard name team
  let Board { boardId = bId } = board
  throwError $ RedirectWithToken ("/board/" <> pack (show bId)) token

joinBoard :: JoinBoardRequest
          -> ChessXServer (Headers '[Header "set-cookie" Token] Board)
joinBoard (JoinBoardRequest bId name) = do
  board <- Game.getBoard bId
  let team = if isNothing (playerWhite board) then White else Black
  (joined, token) <- Game.joinBoard name team board
  return $ addHeader token board

getBoard :: Int -> ChessXServer Board
getBoard = Game.getBoard

selectPiece :: BoardId -> PieceId -> ChessXServer PossibleMoves
selectPiece bId pId = do
  board <- Game.getBoard bId
  Game.validMoves pId board

movePiece :: BoardId
          -> PieceId
          -> PositionCol
          -> PositionRow
          -> ChessXServer Board
movePiece bId pId col row = Game.updateBoard bId $ \board -> do
  (newBoard, piece) <- Game.takePiece pId board
  let newPiece = piece { position = (col, row), hasMoved = True }
  let afterCapture =
        case Game.pieceAt (col, row) board of
          Just p@(Piece { pieceId = i }) | Game.isOpponent newPiece p ->
            Game.removePiece i newBoard
          Nothing ->
            newBoard
  return . Game.toggleTurn
         $ Game.putPiece newPiece afterCapture
