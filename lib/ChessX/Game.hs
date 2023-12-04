module ChessX.Game where

import Debug.Trace

import Control.Monad (replicateM)
import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.ByteString (pack)
import Data.ByteString.Base64 (encodeBase64)
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import System.Random

import ChessX.Board
import ChessX.Error

type ServerState = M.Map BoardId Board
type StateVar = MVar ServerState
type GameT = ReaderT StateVar

-- Functions for running the game:

initStateVar :: MonadIO m => m StateVar
initStateVar = liftIO $ newMVar M.empty

runGame :: MonadIO m => StateVar -> GameT m a -> m a
runGame stateVar g = do
  runReaderT g stateVar

-- Operating on the game state:

withServerState :: (MonadIO m, MonadError Error m)
                => (ServerState -> m (ServerState, a)) -> GameT m a
withServerState updater = do
  stateVar <- ask
  state <- liftIO $ takeMVar stateVar
  (newState, toReturn) <- lift $ updater state
    `catchError` (\e -> do
                     liftIO $ putMVar stateVar state
                     throwError e)
  liftIO $ putMVar stateVar newState
  return toReturn

updateBoard :: (MonadIO m, MonadError Error m)
            => BoardId -> (Board -> m Board) -> GameT m Board
updateBoard bId updater = withServerState $ \state -> do
  let maybeBoard = M.lookup bId state
  board <- maybe (throwError $ NotFound "Board not found") return maybeBoard
  newBoard <- updater board
  return (M.insert bId newBoard state, newBoard)

createBoard :: (MonadIO m, MonadError Error m) => GameT m Board
createBoard = do
  withServerState $
    \state ->
      let bId = maybe 1 ((+1) . fst . fst) $ M.maxViewWithKey state
          board = initialBoard bId
      in return (M.insert bId board state, board)

getBoard :: (MonadIO m, MonadError Error m) => BoardId -> GameT m Board
getBoard bId = withServerState $ \state ->
  case M.lookup bId state of
    Nothing ->
      throwError $ NotFound "Board not found"
    Just board ->
      return (state, board)

findBoard :: (MonadIO m, MonadError Error m) => Token -> GameT m Board
findBoard token = withServerState $ \state ->
  case find hasToken (M.elems state) of
    Nothing ->
      throwError $ NotFound "No board with such token"
    Just board ->
      return (state, board)
  where hasToken board =
          fmap playerToken (playerWhite board) == Just token
          || fmap playerToken (playerBlack board) == Just token

-- Board operations:

joinBoard :: (MonadIO m, MonadError Error m)
          => Text -> Team -> Board -> m (Board, Token)
joinBoard name team board = do
  let tokenLen = 20
  token <- fmap (encodeBase64 . pack) . liftIO $ replicateM tokenLen randomIO
  if | team == White && isNothing (playerWhite board) ->
         return (board { playerWhite = Just (Player name token) }, token)
     | team == Black && isNothing (playerBlack board) ->  
         return (board { playerBlack = Just (Player name token) }, token)
     | otherwise ->
         throwError $ BadRequest "Seat is occupied"

findPiece :: MonadError Error m => PieceId -> Board -> m Piece
findPiece pId board =
  case find (\Piece { pieceId = i } -> i == pId) (pieces board) of
    Nothing ->
      throwError $ NotFound "Piece is not on the board"
    Just p ->
      return p

removePiece :: PieceId -> Board -> Board
removePiece pId board =
  board { pieces = filter (\Piece { pieceId = i } -> i /= pId) (pieces board) }
  
takePiece :: MonadError Error m => PieceId -> Board -> m (Board, Piece)
takePiece pId board = do
  piece <- findPiece pId board
  return (removePiece pId board, piece)

putPiece :: Piece -> Board -> Board
putPiece piece board =
  board { pieces = piece : pieces board }

pieceAt :: Position -> Board -> Maybe Piece
pieceAt pos board =
  find (\Piece { position = p } -> p == pos) (pieces board)

posIsEmpty :: Position -> Board -> Bool
posIsEmpty pos = isNothing . pieceAt pos

isOpponent :: Piece -> Piece -> Bool
isOpponent p1 p2 = team p1 /= team p2

posHasOpponent :: Position -> Piece -> Board -> Bool
posHasOpponent pos p1 board =
  case pieceAt pos board of
    Just p2 -> isOpponent p1 p2
    _ -> False

posHasFriend :: Position -> Piece -> Board -> Bool
posHasFriend pos p1 board =
  case pieceAt pos board of
    Just p2 -> not $ isOpponent p1 p2
    _ -> False

toggleTurn :: Board -> Board
toggleTurn board = board { turn = if turn board == White then Black else White }

initialBoard :: BoardId -> Board
initialBoard bId = Board
  { boardId = bId
  , playerWhite = Nothing
  , playerBlack = Nothing
  , turn = White
  , pieces =
    [ Piece 01 White Rook   (1, 1) False
    , Piece 02 White Knight (2, 1) False
    , Piece 03 White Bishop (3, 1) False
    , Piece 04 White King   (4, 1) False
    , Piece 05 White Queen  (5, 1) False
    , Piece 06 White Bishop (6, 1) False
    , Piece 07 White Knight (7, 1) False
    , Piece 08 White Rook   (8, 1) False
    , Piece 09 White Pawn   (1, 2) False
    , Piece 10 White Pawn   (2, 2) False
    , Piece 11 White Pawn   (3, 2) False
    , Piece 12 White Pawn   (4, 2) False
    , Piece 13 White Pawn   (5, 2) False
    , Piece 14 White Pawn   (6, 2) False
    , Piece 15 White Pawn   (7, 2) False
    , Piece 16 White Pawn   (8, 2) False

    , Piece 17 Black Rook   (1, 8) False
    , Piece 18 Black Knight (2, 8) False
    , Piece 19 Black Bishop (3, 8) False
    , Piece 20 Black King   (4, 8) False
    , Piece 21 Black Queen  (5, 8) False
    , Piece 22 Black Bishop (6, 8) False
    , Piece 23 Black Knight (7, 8) False
    , Piece 24 Black Rook   (8, 8) False
    , Piece 25 Black Pawn   (1, 7) False
    , Piece 26 Black Pawn   (2, 7) False
    , Piece 27 Black Pawn   (3, 7) False
    , Piece 28 Black Pawn   (4, 7) False
    , Piece 29 Black Pawn   (5, 7) False
    , Piece 30 Black Pawn   (6, 7) False
    , Piece 31 Black Pawn   (7, 7) False
    , Piece 32 Black Pawn   (8, 7) False
    ]
  }

validMoves :: MonadError Error m => PieceId -> Board -> m PossibleMoves
validMoves pId board@(Board { boardId = bId }) = do
  piece <- findPiece pId board
  let pos@(col, row) = position piece

  let outsideBoard (c, r) = c < 1 || c > 8 || r < 1 || r > 8

  let walk (c, r) (cdir, rdir) =
        let nextPos = (c+cdir, r+rdir)
        in if | outsideBoard nextPos -> []
              | posHasFriend nextPos piece board -> []
              | posHasOpponent nextPos piece board -> [nextPos]
              | otherwise -> nextPos : walk nextPos (cdir, rdir)

  let predMove pred move = if pred then pure move else mempty
        
  let positions =
        case piece of
          Piece { pieceType = Pawn, team = White, hasMoved = moved } ->
            predMove (posIsEmpty (col, row+1) board) (col, row+1)
            <>
            predMove (not moved &&
                      posIsEmpty (col, row+1) board &&
                      posIsEmpty (col, row+2) board)
                     (col, row+2)
            <>
            predMove (posHasOpponent (col+1, row+1) piece board) (col+1, row+1)
            <>
            predMove (posHasOpponent (col-1, row+1) piece board) (col-1, row+1)
            -- TODO: Do en passant (need a "last move" field for that)

          Piece { pieceType = Pawn, team = Black, hasMoved = moved } ->
            predMove (posIsEmpty (col, row-1) board) (col, row-1)
            <>
            predMove (not moved &&
                      posIsEmpty (col, row-1) board &&
                      posIsEmpty (col, row-2) board)
                     (col, row-2)
            <>
            predMove (posHasOpponent (col+1, row-1) piece board) (col+1, row-1)
            <>
            predMove (posHasOpponent (col-1, row-1) piece board) (col-1, row-1)

          Piece { pieceType = Rook } ->
            concatMap (walk pos) [(-1, 0), (1, 0), (0, -1), (0, 1)]

          Piece { pieceType = Knight } ->
            filter (\p -> not (outsideBoard p || posHasFriend p piece board))
            [(col-1, row+2), (col-1, row-2),
             (col-2, row+1), (col-2, row-1),
             (col+1, row+2), (col+1, row-2),
             (col+2, row+1), (col+2, row-1)]

          Piece { pieceType = Bishop } ->
            concatMap (walk pos) [(-1, -1), (1, -1), (1, 1), (-1, 1)]

          Piece { pieceType = Queen } ->
            concatMap (walk pos) [(-1, 0), (1, 0), (0, -1), (0, 1),
                                  (-1, -1), (1, -1), (1, 1), (-1, 1)]

          Piece { pieceType = King } ->
            filter (\p -> not (outsideBoard p || posHasFriend p piece board))
            [(col-1, row+1), (col, row+1), (col+1, row+1),
             (col-1, row),                 (col+1, row),
             (col-1, row-1), (col, row-1), (col+1, row-1)]
  
  return . PossibleMoves $
    map (\pos -> Move { boardId = bId, pieceId = pId, to = pos }) positions
