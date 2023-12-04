{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module ChessX.API where

import Data.Text (Text)
import GHC.Generics
import Servant.API
import Web.FormUrlEncoded (FromForm)

import ChessX.HTMX
import ChessX.HTMX.Index
import ChessX.HTMX.BoardPage
import ChessX.Board

type API =
       PagesAPI
  :<|>"api" :> "board" :> BoardAPI
  :<|> Raw -- Static files

type PagesAPI =
       Get '[HTMX] Index
  :<|> "board" :> Capture "board_id" Int :>  Get '[HTMX] BoardPage  

type BoardAPI =
       ReqBody '[FormUrlEncoded] CreateBoardRequest
       :> Post '[HTMX] NoContent

  :<|> "join" :> ReqBody '[FormUrlEncoded] JoinBoardRequest
       :> Post '[HTMX] (Headers '[Header "set-cookie" Text] Board)

  :<|> Capture "board_id" BoardId
       :> Get '[HTMX] Board

  :<|> Capture "board_id" BoardId
       :> "select"
       :> Capture "piece_id" Int
       :> Get '[HTMX] PossibleMoves

  :<|> Capture "board_id" BoardId
       :> "move"
       :> Capture "piece_id" Int
       :> Capture "to_column" Int
       :> Capture "to_row" Int
       :> Get '[HTMX] Board

data CreateBoardRequest = CreateBoardRequest
  { playerName :: Text
  , playerTeam :: Team
  }
  deriving (Generic)

instance FromForm CreateBoardRequest

data JoinBoardRequest = JoinBoardRequest
  { boardId :: BoardId
  , playerName :: Text
  }
  deriving (Generic)

instance FromForm JoinBoardRequest