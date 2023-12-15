{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module ChessX.API where

import Data.Text (Text)
import GHC.Generics
import Servant.API
import Web.FormUrlEncoded (FromForm)

import ChessX.HTMX
import ChessX.HTMX.IndexPage
import ChessX.HTMX.BoardPage
import ChessX.Board
import ChessX.Token

type API =
        PagesAPI
  :<|> "api" :> "board" :> BoardAPI
  :<|> "sse" :> SseAPI
  :<|> Raw -- Static files

type PagesAPI =
       Get '[HTMX] IndexPage
  :<|> "board"
     :> QueryParam' '[Required] "board_id" Int
     :> QueryParam' '[Optional] "as" Team
     :> Get '[HTMX] BoardPage

type BoardAPI =
       ReqBody '[FormUrlEncoded] CreateBoardRequest
       :> Post '[HTMX] (Headers
                         '[Header "HX-Location" Text]
                         NoContent)

  :<|> "join" :> ReqBody '[FormUrlEncoded] JoinBoardRequest
       :> Post '[HTMX] (Headers
                         '[Header "HX-Location" Text]
                         NoContent)

  :<|> Capture "board_id" BoardId
       :> QueryParam' '[Optional] "as" Team
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

type SseAPI = Capture "board_id" BoardId :> RawM

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
