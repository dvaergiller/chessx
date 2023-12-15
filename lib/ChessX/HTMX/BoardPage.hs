{-# LANGUAGE QuasiQuotes #-}
module ChessX.HTMX.BoardPage where

import IHP.HSX.QQ
import Servant.API (Accept(..))
import Text.Blaze.Html5

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
    in [hsx|
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8" />
  <meta name="description" content="HTMX Demo Chess Game" />
  <meta name="viewport" content="width=device-width" />
  <script src="https://unpkg.com/htmx.org@1.9.9"></script>
  <script src="https://unpkg.com/htmx.org/dist/ext/sse.js"></script>
  <link rel="stylesheet" href="/reset.css"/>
  <link rel="stylesheet" href="/index.css"/>
  <title>ChessX</title>
</head>
<body>
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
</body>
</html>
|]
