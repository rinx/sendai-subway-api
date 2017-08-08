{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Web.Spock
import Web.Spock.Config
import Web.Spock.Action

import Network.HTTP.Types
import GHC.Generics (Generic)

import Control.Monad.IO.Class
import Data.Monoid
import Data.Maybe
import Data.IORef
import qualified Data.Text as T

import Data.Aeson hiding (json)

import Database.PostgreSQL.Simple as PSQL

import Lib

data MySession = EmptySesssion
data MyAppState = DummyAppState (IORef Int)

main :: IO ()
main = do
    let psqlConnect = PSQL.connect PSQL.defaultConnectInfo {PSQL.connectDatabase = "ssubapi"}
        dbConn = ConnBuilder psqlConnect PSQL.close (PoolCfg 1 1 30)
    ref <- newIORef 0
    spockCfg <- defaultSpockCfg EmptySesssion (PCConn dbConn) (DummyAppState ref)
    runSpock 8080 (spock spockCfg app)

app :: SpockM Connection MySession MyAppState ()
app = do
    get root $ text ""
    get "station" $ do
        stations <- liftIO $ getStations
        json $ stations
    get ("station" <//> var) $ \code -> do
        res <- liftIO $ getStationData code
        json $ res

