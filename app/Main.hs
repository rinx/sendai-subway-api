{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Web.Spock
import Web.Spock.Config

import Network.HTTP.Types
import GHC.Generics (Generic)

import Control.Monad.Trans
import Data.Monoid
import Data.Maybe
import Data.IORef
import qualified Data.Text as T

import Data.Aeson hiding (json)

import Lib

data MySession = EmptySesssion
data MyAppState = DummyAppState (IORef Int)

main :: IO ()
main = do
    ref <- newIORef 0
    spockCfg <- defaultSpockCfg EmptySesssion PCNoDatabase (DummyAppState ref)
    runSpock 8080 (spock spockCfg app)

app :: SpockM () MySession MyAppState ()
app = do
    get root $ text ""
    get ("station" <//> var) $ \code -> do
        json $ getStationData code

