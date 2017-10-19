{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Monad.Reader      (runReaderT)
import           Data.Pool                 (Pool, withResource)
import           Database.MySQL.Simple     (Connection, execute, execute_)
import           Network.HTTP.Types.Status (created201, notFound404, ok200)
import           Network.Wai               (Application)
import           Network.Wai.Internal      (requestMethod)
import           Network.Wai.Test          (SRequest (SRequest), defaultRequest,
                                            request, runSession, setPath,
                                            simpleBody, simpleHeaders,
                                            simpleRequest, simpleRequestBody,
                                            simpleStatus, srequest)
import           Test.Hspec                (before, describe, hspec, it,
                                            shouldBe)

import           Web.Scotty.Trans          (scottyAppT)

import           App
import           Config
import           Types

main :: IO ()
main = do
  c <- getConfig >>= setEnv Test
  let e = env c
      t m = runReaderT (runConfigM m) c
      p = pool c
  a <- scottyAppT t (app e)
  hspec $
    before (withResource p resetDb) $ do
      describe "GET /" $ do
        it "200" $ do
          r <- runSession (request defaultRequest) a
          simpleStatus r `shouldBe` ok200
          simpleBody r `shouldBe` "Test"
      describe "GET /notFound" $ do
        it "404" $ do
          r <- runSession (request (defaultRequest `setPath` "/notFound")) a
          simpleStatus r `shouldBe` notFound404
          simpleBody r `shouldBe` "<h1>404 - Not Found</h1>"
      describe "GET /todos" $ do
        it "200" $ do
          r <- runSession (request (defaultRequest `setPath` "/todos")) a
          simpleStatus r `shouldBe` ok200
      describe "GET /todos/1" $ do
        it "200" $ do
          r <- runSession (request (defaultRequest `setPath` "/todos/1")) a
          simpleStatus r `shouldBe` ok200
      describe "POST /todos" $ do
        it "201" $ do
          let req =
                srequest
                  SRequest
                  { simpleRequest =
                      defaultRequest {requestMethod = "POST"} `setPath` "/todos"
                  , simpleRequestBody =
                      "{ \"name\": \"test-1\", \"status\": \"Active\" }"
                  }
          r <- runSession req a
          simpleStatus r `shouldBe` created201
      describe "PUT /todos/1" $ do
        it "200" $ do
          let req =
                srequest
                  SRequest
                  { simpleRequest =
                      defaultRequest {requestMethod = "PUT"} `setPath`
                      "/todos/1"
                  , simpleRequestBody =
                      "{ \"name\": \"new-test-1\", \"status\": \"Complete\" }"
                  }
          r <- runSession req a
          simpleStatus r `shouldBe` ok200
          simpleBody r `shouldBe`
            "{\"status\":\"Complete\",\"name\":\"new-test-1\",\"id\":1}"
      describe "DELETE /todos/1" $ do
        it "200" $ do
          let req =
                srequest
                  SRequest
                  { simpleRequest =
                      defaultRequest {requestMethod = "DELETE"} `setPath`
                      "/todos/1"
                  }
          r <- runSession req a
          simpleStatus r `shouldBe` ok200

resetDb :: Connection -> IO ()
resetDb c = do
  execute_ c "drop table if exists todo"
  execute_ c "create table todo (id int(11) not null auto_increment, name varchar(255) not null, status varchar(255) not null, primary key (id))"
  execute_ c "insert into todo (name,status) values('item-1','Active'),('item-2','Complate')"
  return ()
