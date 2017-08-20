{-# LANGUAGE DeriveGeneric #-}

module Subway.Types
    ( Station(..)
    , TimeTableData(..)
    , StrHour
    , StrMin
    , StaCode
    , StaDest
    , RawTimeTable(..)
    , TimeTable(..)
    , StationData(..)
    ) where

import GHC.Generics (Generic)
import Data.Aeson

data Station = Station
    { code :: Int
    , name :: String
    } deriving (Generic, Show)

data TimeTableData = TimeTableData
    { station :: String
    , destination :: String
    , timetable :: [TimeTable]
    } deriving (Generic, Show)

type StrHour = String
type StrMin  = String
type StaCode = String
type StaDest = [(String, String)]

data RawTimeTable = RawTimeTable [StrMin] StrHour [StrMin]
    deriving (Generic, Show)
data TimeTable = TimeTable
    { hour :: StrHour
    , mins :: [StrMin]
    } deriving (Generic, Show)
data StationData = StationData StaCode StaDest
    deriving (Generic, Show)

instance ToJSON Station
instance ToJSON TimeTableData
instance ToJSON TimeTable
instance ToJSON StationData
