{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main (main) where

import           Control.Monad.Reader      (runReaderT)
import           Network.HTTP.Types.Status (notFound404, ok200)
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
  a <- scottyAppT t (app e)
  hspec $ do
    describe "GET /" $ do
      it "200" $ do
        r <- runSession (request defaultRequest) a
        simpleStatus r `shouldBe` ok200
        simpleBody r `shouldBe` "Test"
    describe "GET /notFound" $ do
      it "404" $ do
        r <- runSession (request (defaultRequest `setPath` "/notFound")) a
        simpleStatus r `shouldBe` notFound404
        simpleBody r `shouldBe` "null"
