module ChessX.Board where

import Data.List (zip, lookup)
import Data.Maybe (fromMaybe)
import Data.Text (Text, singleton, pack)
import Web.Internal.HttpApiData (FromHttpApiData(..))

data PieceType =
    Pawn
  | Rook
  | Knight
  | Bishop
  | Queen
  | King
  deriving (Show, Eq)

data Team = White | Black
  deriving (Show, Eq)

instance FromHttpApiData Team where
  parseUrlPiece = \case
    "white" -> Right White
    "black" -> Right Black
    _ -> Left "Invalid team. Needs to be 'black' or 'white'"

type PositionRow = Int
type PositionCol = Int
type Position = (PositionCol, PositionRow)

rowName :: Position -> Text
rowName (_, row) = 
  fromMaybe "unknown" . lookup row $ zip [1..8] (map singleton ['1'..'8'])

columnName :: Position -> Text
columnName (col, _) =
  fromMaybe "unknown" . lookup col $ zip [1..8] (map singleton ['A'..'H'])

columnNum :: Position -> Text
columnNum (col, _) = pack (show col)

type PieceId = Int
data Piece = Piece
  { pieceId :: PieceId
  , team :: Team
  , pieceType :: PieceType
  , position :: Position
  , hasMoved :: Bool
  }

type Token = Text

data Player = Player
  { playerName :: Text,
    playerToken :: Token
  }

type BoardId = Int
data Board = Board
  { boardId :: BoardId
  , playerWhite :: Maybe Player
  , playerBlack :: Maybe Player
  , pieces :: [Piece]
  , turn :: Team
  }

data Move = Move
  { boardId :: BoardId
  , pieceId :: PieceId
  , to :: Position
  }

newtype PossibleMoves = PossibleMoves { moves :: [Move] }