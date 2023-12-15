{-# LANGUAGE QuasiQuotes #-}
module ChessX.HTMX.Index where

import IHP.HSX.QQ
import Servant.API (Accept(..))
import Text.Blaze.Html5

data Index = Index

instance ToMarkup Index where
  toMarkup Index =
    [hsx|
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
    <main>
    <div class="main-menu">
      <input class="custom-radio tab-switch"
             id="create"
             type="radio"
             name="create-or-join"
             checked/>
      <label class="menu-tab-label"
             for="create"
             role="tab"
             tabindex="0">
        Create New
      </label>

      <form id="create-form"
            class="tab-content main-menu-form"
            hx-post="/api/board"
            hx-push-url="true"
            hx-target="closest body">

        <label for="player-name">Your name</label>
        <input id="player-name"
               class="main-menu-text"
               name="playerName"
               required />
        <input class="custom-radio"
               name="playerTeam"
               type="radio"
               id="white"
               value="white" />
        <label id="team-select-white" class="team-select-label" for="white"></label>
        <input class="custom-radio"
               name="playerTeam"
               type="radio"
               id="black"
               value="black" />
        <label id="team-select-black" class="team-select-label" for="black"></label>
        <button type="submit">Create Board</button>
      </form>
      <input class="custom-radio tab-switch"
             id="join"
             type="radio"
             name="create-or-join"/>
      <label class="menu-tab-label"
             for="join"
             role="tab"
             tabindex="1">
        Join
      </label>
       <form id="join-form"
            class="tab-content main-menu-form"
            hx-post="/api/board/join"
            hx-target="main"
            hx-push-url="/board.html">
         <label for="player-name">Your name</label>
        <input id="player-name" class="main-menu-text" name="playerName" required />
        <label for="board-id">Board ID</label>
        <input id="board-id" class="main-menu-text" name="boardId" required />
        <button type="submit">Join Board</button>
      </form>
    </div>
  </main>
</body>
</html>
|]
