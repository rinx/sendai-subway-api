{-# LANGUAGE DeriveGeneric #-}

module Subway.Types
    ( Station(..)
    , StrHour
    , StrMin
    , StaCode
    , StaDest
    , TimeTable(..)
    , StationData(..)
    ) where

import GHC.Generics (Generic)
import Data.Aeson

data Station = Station
    { code :: Int
    , name :: String
    } deriving (Generic, Show)

type StrHour = String
type StrMin  = String
type StaCode = String
type StaDest = [(String, String)]

data TimeTable = TimeTable [StrMin] StrHour [StrMin]
data StationData = StationData StaCode StaDest

instance ToJSON Station
