{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module App (runApp) where

import           Config
import           Types

import           Control.Monad.Reader                 (MonadIO, MonadTrans, ask,
                                                       asks, lift, liftIO,
                                                       runReaderT)
import           Data.Aeson                           (Value (Null), object,
                                                       (.=))
import           Data.Text.Lazy                       (pack)
import           Network.HTTP.Types                   (internalServerError500,
                                                       notFound404)
import           Network.Wai                          (Middleware)
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import           Web.Scotty.Trans                     (ScottyT, defaultHandler,
                                                       get, json, middleware,
                                                       notFound, scottyOptsT,
                                                       scottyT, showError,
                                                       status, text)

notFoundA :: Action
notFoundA = do
  status notFound404
  json Null

loggingM :: Env -> Middleware
loggingM Dev  = logStdoutDev
loggingM Prod = logStdout
loggingM Test = id

defaultH :: Env -> Error -> Action
defaultH e x = do
  status internalServerError500
  json $ case e of
    Dev  -> object ["error" .= showError x]
    Prod -> Null
    Test -> object ["error" .= showError x]

app :: Env -> ScottyT Error ConfigM ()
app e = do
  middleware $ loggingM e
  defaultHandler $ defaultH e
  get "/" $ do
    e <- lift $ asks env
    text . pack $ show e
  notFound notFoundA

runApp :: IO ()
runApp = do
  c <- getConfig
  o <- getOptions (env c)
  let r m = runReaderT (runConfigM m) c
  scottyOptsT o r (app $ env c)
