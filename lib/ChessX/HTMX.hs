module ChessX.HTMX where

import Prelude hiding (id, div)

import Control.Monad (forM_)
import Data.Char (toLower)
import Data.List (intercalate)
import Data.String (fromString)
import qualified Data.Text as T (Text, pack, unpack, toLower, intercalate)
import Network.HTTP.Media ((//), (/:))
import Servant
import Servant.API (Accept(..))
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import Text.Blaze.Html.Renderer.Utf8

import ChessX.Board

data HTMX = HTMX

instance Accept HTMX where
  contentType _ = "text" // "html"

instance ToMarkup a => MimeRender HTMX a where
  mimeRender _ val = renderHtml $ toHtml val

instance ToMarkup Board where
  toMarkup board@(Board { boardId = bId, turn = trn, pieces = ps }) =
    div ! class_ "board"
        ! attr "turn" trn
        ! onclick "htmx.findAll('.moves').forEach(e => e.remove())" $ do
        playerMarkup (playerWhite board)
        playerMarkup (playerBlack board)
        div ! class_ "pieces" $ forM_ ps pieceMarkup
    where pieceMarkup piece@(Piece {pieceId = pId, position = pos}) =
            div ! class_ "piece"
                ! id (fromString ("piece-" <> show pId))
                ! textAttr "column" (columnName pos)
                ! textAttr "row" (rowName pos)
                ! attr "type" (pieceType piece)
                ! attr "team" (team piece)
                !? (trn == team piece, hx "get" (selectUrl piece))
                !? (trn == team piece, hx "target" "closest .board")
                !? (trn == team piece, hx "swap" "beforeend")
                $ mempty
          selectUrl piece@(Piece { pieceId = pId }) = T.intercalate "/"
            [ "/api/board"
            , T.pack $ show bId
            , "select"
            , T.pack $ show pId
            ]
          playerMarkup Nothing = return ()
          playerMarkup (Just (Player name team)) = mempty

instance ToMarkup PossibleMoves where
  toMarkup possibleMoves =
    div ! class_ "moves" $ forM_ (moves possibleMoves) toMarkup

instance ToMarkup Move where
  toMarkup (Move { boardId = bId, pieceId = pId, to = t }) =
    div ! class_ "move"
        ! textAttr "column" (columnName t)
        ! textAttr "row" (rowName t)
        ! hx "get" moveUri
        ! hx "target" "closest .board"
        ! hx "swap" "outerHTML"
        $ mempty
    where moveUri = T.intercalate "/"
            [ "/api/board"
            , T.pack $ show bId
            , "move"
            , T.pack $ show pId
            , columnNum t
            , rowName t
            ]
    

attr :: (Show a) => Tag -> a -> Attribute
attr name = dataAttribute name . toValue . fmap toLower . show

textAttr :: Tag -> T.Text -> Attribute
textAttr name = dataAttribute name . toValue . T.toLower

hx :: T.Text -> T.Text -> Attribute
hx hxAttr value =
  customAttribute (textTag $ "hx-" <> hxAttr) $ fromString (T.unpack value)
