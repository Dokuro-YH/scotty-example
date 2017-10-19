{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module App (app, runApp) where

import           Config
import qualified Db                                   as DB
import           Types

import           Control.Monad.Reader

import           Data.Aeson                           (Value (Null), object,
                                                       (.=))
import           Data.Pool                            (withResource)
import qualified Data.Text.Lazy                       as L

import           Database.MySQL.Simple                (query, query_)
import           Database.MySQL.Simple.QueryParams    (QueryParams)
import           Database.MySQL.Simple.QueryResults   (QueryResults)
import           Database.MySQL.Simple.Types          (Only)

import           Network.HTTP.Types                   (created201,
                                                       internalServerError500,
                                                       notFound404)
import           Network.Wai                          (Middleware)
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)

import           Web.Scotty                           (ActionM)
import           Web.Scotty.Trans

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

listTodoA :: Action
listTodoA = do
  p <- lift $ asks pool
  r <- liftIO $ withResource p DB.selectTodoList
  json r

getTodoA :: Action
getTodoA = do
  id' <- param "id"
  p <- lift $ asks pool
  r <- liftIO $ withResource p $ DB.selectTodo id'
  json r

addTodoA :: Action
addTodoA = do
  q <- jsonData
  p <- lift $ asks pool
  r <- liftIO $ withResource p $ DB.insertTodo q
  status created201
  json r

updateTodoA :: Action
updateTodoA = do
  id' <- param "id"
  q <- jsonData
  p <- lift $ asks pool
  r <- liftIO $ withResource p $ DB.updateTodo q id'
  json r

deleteTodoA :: Action
deleteTodoA = do
  id' <- param "id"
  p <- lift $ asks pool
  liftIO $ withResource p $ DB.deleteTodo id'

notFoundA :: Action
notFoundA = do
  status notFound404
  html "<h1>404 - Not Found</h1>"

app :: Env -> ScottyT Error ConfigM ()
app e = do
  middleware $ loggingM e
  defaultHandler $ defaultH e
  get "/" $ do
    e <- lift $ asks env
    text . L.pack $ show e
  get "/todos" listTodoA
  get "/todos/:id" getTodoA
  post "/todos" addTodoA
  put "/todos/:id" updateTodoA
  delete "/todos/:id" deleteTodoA
  notFound notFoundA

runApp :: IO ()
runApp = do
  c <- getConfig
  o <- getOptions (env c)
  let r m = runReaderT (runConfigM m) c
      a = app $ env c
  scottyOptsT o r a
