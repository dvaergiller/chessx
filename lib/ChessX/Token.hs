{-# LANGUAGE DataKinds #-}
module ChessX.Token
( Token(..),
  TokenCookie(..),
  TokenCookieHeader,
  mkNewToken
)
where

import Control.Monad (replicateM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (pack, unpack)
import Data.ByteString.Base64 (encodeBase64)
import Data.List (find)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Servant
import System.Random (randomIO)
import Web.Cookie

newtype Token = Token { tokenText :: Text }
  deriving (Show)

newtype TokenCookie = TokenCookie { toToken :: Token }
  deriving (Show)

instance ToHttpApiData TokenCookie where
  toUrlPiece token =
    "token=" <> tokenText (toToken token) <> ";SameSite=strict"

type TokenCookieHeader = Header "Set-Cookie" TokenCookie

instance FromHttpApiData Token where
  parseUrlPiece cookieHeader =
    case lookup "token" . parseCookiesText $ encodeUtf8 cookieHeader of
      Nothing ->
        Left "No token cookie"
      Just tokenCookie ->
        Right $ Token tokenCookie

mkNewToken :: MonadIO m => m Token
mkNewToken =
  let tokenLen = 20
  in fmap (Token . encodeBase64 . pack) . liftIO $ replicateM tokenLen randomIO
