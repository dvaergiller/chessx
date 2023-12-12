module ChessX.Error
( module ChessX.Error
, module Control.Monad.Except
)
where

import Control.Monad.Except
import Control.Monad.Trans
import Data.Bifunctor
import Data.ByteString (fromStrict)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Servant.Server

import ChessX.Board

type ErrorT = ExceptT Error 

data Error =
    NotFound Text
  | BadRequest Text

instance {-# OVERLAPPABLE #-}
  (Monad (t m),
   MonadTrans t,
   MonadError e m)
   => MonadError e (t m) where
  throwError e = lift (throwError e)
  catchError = catchError

sendError :: (Monad m, MonadError Error m) => Error -> m ()
sendError = throwError

toServerError :: MonadError ServerError m => ExceptT Error m a -> m a
toServerError valM = do
  res <- runExceptT valM
  case res of
    Left (NotFound msg) ->
      throwError $ err404 { errBody = fromStrict $ encodeUtf8 msg }
    Left (BadRequest msg) ->
      throwError $ err400 { errBody = fromStrict $ encodeUtf8 msg }
    Right val ->
      return val
