{-# LANGUAGE DataKinds #-}
module ChessX.Server where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import Data.List
import Data.Maybe (isNothing)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Network.Wai (Middleware, Request(..), mapResponseHeaders)
import Servant

import ChessX.API
import ChessX.Board
import ChessX.Error
import qualified ChessX.Game as Game
import ChessX.HTMX.Index
import ChessX.HTMX.BoardPage
import ChessX.Token
import Web.Cookie
import Debug.Trace

type ChessXServer = Game.GameT (ErrorT Handler)

application :: IO Application
application = do
  stateVar <- Game.initStateVar
  let app = serve api $ hoistServer api (toHandler stateVar) server
  return $ tokenMiddleware app

-- Sets a token response header in all responses,
-- reusing the same one if already set.
tokenMiddleware :: Application -> Application
tokenMiddleware app req respond =
  let existingHeader = lookup "Cookie" (requestHeaders req)
      existingToken = existingHeader >>= lookup "token" . parseCookies
      respond' resp =
        case existingToken of
          Nothing -> do
            token <- mkNewToken
            let tokenCookie = encodeUtf8 . toUrlPiece $ TokenCookie token
            let tokenHeader = ("Set-Cookie", tokenCookie)
            respond $ mapResponseHeaders (tokenHeader:) resp
          Just _ ->
            respond resp
  in app req respond'

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
         :<|> \bId asTeam -> return (BoardPage bId asTeam)

boardServer :: ServerT BoardAPI ChessXServer
boardServer = makeNewBoard
         :<|> joinBoard
         :<|> getBoard
         :<|> selectPiece
         :<|> movePiece

makeNewBoard ::
     CreateBoardRequest
  -> ChessXServer
       (Headers '[Header "HX-Location" Text] NoContent)
makeNewBoard (CreateBoardRequest name team) = do
  board@(Board { boardId = bId }) <- Game.createBoard
  token <- Game.withBoard bId $ Game.joinBoard name team
  return $ addHeader  ("/board?board_id=" <> pack (show bId)
                       <> "&as=" <> toUrlPiece team)
           NoContent

joinBoard ::
     JoinBoardRequest
  -> ChessXServer
       (Headers '[Header "HX-Location" Text] NoContent)
joinBoard (JoinBoardRequest bId name) = do
  Game.withBoard bId $ \board -> do
    let team = if isNothing (playerWhite board) then White else Black
    (joined, token) <- Game.joinBoard name team board
    trace (show joined) (return ())
    return (joined, addHeader  ("/board?board_id=" <> pack (show bId)
                                  <> "&as=" <> toUrlPiece team)
                    NoContent)

getBoard :: Int -> ChessXServer Board
getBoard =
  Game.getBoard

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
