{-# LANGUAGE QuasiQuotes #-}
module ChessX.HTMX.BoardPage where

import IHP.HSX.QQ
import Servant.API (Accept(..))
import Text.Blaze.Html5

import ChessX.HTMX.PageTmpl
import ChessX.Board

data BoardPage = BoardPage
  { boardId :: BoardId
  , asTeam :: Maybe Team
  }

instance ToMarkup BoardPage where
  toMarkup (BoardPage bId asTeam) =
    let asTeamStr =
          case asTeam of
            Just White -> "white" :: String
            Just Black -> "black"
            Nothing -> "none"
    in pageWith [hsx|
         <main hx-ext="sse"
               sse-connect={ "/sse/" ++ show bId } >
           <div class="game-state"
                data-as-team={ asTeamStr } >
           </div>
           <div class="board"
                hx-get={ "/api/board/" ++ show bId ++ "?as=" ++ asTeamStr}
                hx-swap="outerHTML"
                hx-trigger="load"></div>
         </main>
         |]
