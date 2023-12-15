{-# LANGUAGE QuasiQuotes #-}
module ChessX.HTMX.PageTmpl where

import IHP.HSX.QQ
import Servant.API (Accept(..))
import Text.Blaze.Html5

pageWith :: Markup -> Markup
pageWith contents =
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
            { contents }
          </body>
        </html>
    |]
