module Config (getConfig, getOptions, setEnv) where

import           Types

import           Control.Applicative
import           Control.Monad.Reader
import           Data.Default             (def)
import           Data.Maybe               (fromMaybe)
import           Data.Pool                (Pool, createPool)
import           Database.MySQL.Simple    (ConnectInfo (..), Connection, close,
                                           connect, defaultConnectInfo)
import           Network.Wai.Handler.Warp (Settings, defaultSettings,
                                           setFdCacheDuration, setPort)
import           System.Environment       (lookupEnv)
import           Web.Scotty.Trans         (Options, settings, verbose)


getOptions :: Env -> IO Options
getOptions e = do
  s <- getSettings e
  let v = case e of
        Dev  -> 1
        Prod -> 0
        Test -> 0
  return def { settings = s
             , verbose = v
             }


getConfig :: IO Config
getConfig = do
  env <- getEnv
  pool <- getPool env
  return $ Config { env = env
                  , pool = pool
                  }

setEnv :: Env -> Config -> IO Config
setEnv e c = return Config { env = e
                           , pool = pool c
                           }

getPool :: Env -> IO ConnectionPool
getPool _ = do
  h <- lookupEnv "MYSQL_HOST"
  d <- lookupEnv "MYSQL_DATABASE"
  u <- lookupEnv "MYSQL_USER"
  p <- lookupEnv "MYSQL_PASSWORD"
  let dbConf' = DbConfig <$> h <*> d <*> u <*> p
      c = fromMaybe dbConf dbConf'
  createPool (newConn c) close 1 64 10
  where
    dbConf =
      DbConfig
      { dbHost = "localhost"
      , dbDatabase = "test"
      , dbUser = "root"
      , dbPassword = "root"
      }

newConn :: DbConfig -> IO Connection
newConn conf = connect defaultConnectInfo { connectHost = dbHost conf
                                          , connectDatabase = dbDatabase conf
                                          , connectUser = dbUser conf
                                          , connectPassword = dbPassword conf
                                          }

getSettings :: Env -> IO Settings
getSettings e = do
  let s = defaultSettings
      s' = case e of
        Dev  -> setFdCacheDuration 0 s
        Prod -> s
        Test -> s
  p <- getPort
  return $ setPort p s'

getPort :: IO Int
getPort = do
  p <- lookupEnv "PORT"
  return $ case p of
    Nothing -> 8080
    Just p' -> read p'

getEnv :: IO Env
getEnv = do
  env <- lookupEnv "SCOTTY_ENV"
  return $ case env of
    Nothing -> Dev
    Just e  -> read e
