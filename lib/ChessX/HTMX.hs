{-# LANGUAGE QuasiQuotes #-}
module ChessX.HTMX where

import Prelude hiding (id, div)

import Control.Monad (forM_)
import Data.Char (toLower)
import Data.List (intercalate)
import Data.String (IsString, fromString)
import qualified Data.Text as T (Text, pack, unpack, toLower, intercalate)
import IHP.HSX.QQ
import Network.HTTP.Media ((//), (/:))
import Servant
import Servant.API (Accept(..))
import Text.Blaze.Html5
import Text.Blaze.Html.Renderer.Utf8

import ChessX.Board

data HTMX = HTMX

instance Accept HTMX where
  contentType _ = "text" // "html"

instance ToMarkup a => MimeRender HTMX a where
  mimeRender _ val = renderHtml $ toHtml val

instance ToMarkup Board where
  toMarkup board@(Board { boardId = bId, turn = trn, pieces = ps }) =
    [hsx|
      <div class="board"
           data-turn={ toUrlPiece trn }
           hx-trigger="sse:gameUpdated"
           hx-get={ "/api/board/" <> T.pack (show bId) <> viewAsQs }
           hx-swap="outerHTML"
           onclick="htmx.findAll('.moves').forEach(e => e.remove())">
        <div class="pieces">
          { fmap pieceMarkup ps }
        </div>
      </div>
    |]
      -- { playerMarkup (playerWhite board) }
      -- { playerMarkup (playerBlack board) }
    where pieceMarkup piece@(Piece {pieceId = pId, position = pos}) =
            [hsx|
              <div class="piece"
                   id={ "piece-" ++ show pId }
                   data-column={ columnName pos }
                   data-row={ rowName pos }
                   data-type={ toUrlPiece (pieceType piece) }
                   data-team={ toUrlPiece (team piece) }
                   hx-get={ ifSelectable piece (selectUrl piece) }
                   hx-target={ ifSelectable piece "closest .board" }
                   hx-swap={ ifSelectable piece "beforeend" }>
              </div>
             |]
          selectUrl piece@(Piece { pieceId = pId }) = T.intercalate "/"
            [ "/api/board"
            , T.pack $ show bId
            , "select"
            , T.pack $ show pId
            ]
          -- playerMarkup Nothing = return ()
          -- playerMarkup (Just (Player name team)) = mempty
          ifSelectable :: Piece -> T.Text -> Maybe T.Text
          ifSelectable piece thenDisplay =
            if viewAs board == Just trn && trn == team piece
            then Just thenDisplay
            else Nothing
          viewAsQs = case viewAs board of
                       Nothing -> ""
                       Just White -> "?as=white"
                       Just Black -> "?as=black"

instance ToMarkup PossibleMoves where
  toMarkup possibleMoves =
    [hsx|<div class="moves">{ fmap toMarkup (moves possibleMoves) }</div>|]

instance ToMarkup Move where
  toMarkup (Move { boardId = bId, pieceId = pId, to = t }) =
    [hsx|
        <div class="move"
             data-column={ columnName t }
             data-row={ rowName t }
             hx-get={ moveUri }
             hx-target="closest .board"
             hx-swap="outerHTML">
        </div>
    |]
    where moveUri = T.intercalate "/"
            [ "/api/board"
            , T.pack $ show bId
            , "move"
            , T.pack $ show pId
            , columnNum t
            , rowName t
            ]
