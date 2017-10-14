{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module App (runApp) where

import           Data.Aeson       (Value (..), object, (.=))
import           Network.Wai      (Application)
import           Web.Scotty       (ScottyM, scotty)
import           Web.Scotty.Trans (get, json, text)

app' :: ScottyM ()
app' = do
  get "/" $ do
    text "hello"

  get "/some-json" $ do
    json $ object ["foo" .= Number 23, "bar" .= Number 42]

runApp :: IO ()
runApp = scotty 8080 app'
