{-# LANGUAGE QuasiQuotes #-}
module ChessX.HTMX.IndexPage where

import IHP.HSX.QQ
import Servant.API (Accept(..))
import Text.Blaze.Html5

import ChessX.HTMX.PageTmpl

data IndexPage = IndexPage

instance ToMarkup IndexPage where
  toMarkup IndexPage = pageWith
    [hsx|
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

        <div class="input-group">
          <label for="player-name">Your name</label>
          <input id="player-name"
                 class="main-menu-text"
                 name="playerName"
                 required />
        </div>
        <div class="team-select">
          <input class="custom-radio"
                 name="playerTeam"
                 type="radio"
                 id="white"
                 value="white" />
          <label id="team-select-white"
                 class="team-select-label"
                 for="white"></label>
          <input class="custom-radio"
                 name="playerTeam"
                 type="radio"
                 id="black"
                 value="black" />
          <label id="team-select-black"
                 class="team-select-label"
                 for="black"></label>
        </div>
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
        <div class="input-group">
          <label for="player-name">Your name</label>
          <input id="player-name"
                 class="main-menu-text"
                 name="playerName" required />
        </div>
        <div class="input-group">
          <label for="board-id">Board ID</label>
          <input id="board-id" class="main-menu-text" name="boardId" required />
        </div>
        <button type="submit">Join Board</button>
      </form>
    </div>
  </main>
|]
