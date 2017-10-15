{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import           Control.Applicative
import           Control.Monad.IO.Class   (MonadIO)
import           Control.Monad.Reader     (MonadReader, ReaderT)
import           Data.Pool                (Pool)
import           Data.Text.Lazy           (Text)
import           Database.MySQL.Simple    (ConnectInfo (..), Connection)
import           Network.Wai.Handler.Warp (Settings)
import           Web.Scotty.Trans         (ActionT, Options)

newtype ConfigM a = ConfigM { runConfigM :: ReaderT Config IO a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader Config)

type Error = Text

type ConnectionPool = Pool Connection

type Action = ActionT Error ConfigM ()

data Env = Dev
         | Prod
         | Test
         deriving (Eq, Show, Read)

data Config = Config { env  :: Env
                     , pool :: ConnectionPool
                     }

data DbConfig = DbConfig { dbHost     :: String
                         , dbDatabase :: String
                         , dbUser     :: String
                         , dbPassword :: String
                         }
