{-# LANGUAGE OverloadedStrings #-}

module Subway
    ( getStations
    , getStationData
    ) where

import Subway.Types

import qualified Data.Text as T

import Text.HTML.Scalpel

import Control.Applicative
import Control.Monad
import Control.Concurrent

import Data.Time
import Data.Time.LocalTime
import Data.Time.Calendar.WeekDate
import Data.String.Utils
import Data.List

getStations :: IO [Station]
getStations = return stationList

stationList :: [Station]
stationList =
    [ Station 9285 "仙台"
    , Station 6646 "泉中央"
    , Station 6647 "八乙女"
    , Station 6648 "黒松"
    , Station 6649 "旭ケ丘"
    , Station 6650 "台原"
    , Station 1222 "北仙台"
    , Station 6651 "北四番丁"
    , Station 6652 "勾当台公園"
    , Station 6653 "広瀬通"
    , Station 6654 "五橋"
    , Station 6655 "愛宕橋"
    , Station 6656 "河原町"
    , Station 6657 "長町一丁目"
    , Station 975  "長町"
    , Station 6658 "長町南"
    , Station 6659 "富沢"
    , Station 9727 "八木山動物公園"
    , Station 9728 "青葉山"
    , Station 9729 "川内"
    , Station 9730 "国際センター"
    , Station 9731 "大町西公園"
    , Station 9732 "青葉通一番町"
    , Station 9733 "宮城野通"
    , Station 9734 "連坊"
    , Station 9735 "薬師堂"
    , Station 9736 "卸町"
    , Station 9737 "六丁の目"
    , Station 9738 "荒井"
    ]

getStationNameByCode :: Int -> String
getStationNameByCode code = getStationNameByCode' stationList code
    where
        getStationNameByCode' [] _ = ""
        getStationNameByCode' (x:xs) c =
            let (Station c' name) = x in
                if c' == c
                    then name
                    else getStationNameByCode' xs c

getStationData :: String -> IO [TimeTableData]
getStationData x = comSubway x

comSubway :: String -> IO [TimeTableData]
comSubway ss = do
    ctz <- getCurrentTimeZone
    ct <- getCurrentTime
    lochour <- return $ todHour . localTimeOfDay $ utcToLocalTime ctz ct
    locmin  <- return $ todMin . localTimeOfDay $ utcToLocalTime ctz ct
    (StationData sc ds) <- return $ getStaCode ss
    res <- comSubway' ct ctz sc ds
    return $ res

comSubway' :: UTCTime -> TimeZone -> StaCode -> StaDest -> IO [TimeTableData]
comSubway' _ _ _ [] = return $ []
comSubway' ct ctz sc (d:ds) = do
    let url = mkURL sc $ fst d
    tt <- scrapeTT ct url
    ts <- comSubway' ct ctz sc ds
    return $
        if not $ null tt
            then (TimeTableData (getStationNameByCode $ read sc) (snd d) tt):ts
            else []

scrapeTT :: UTCTime -> String -> IO [TimeTable]
scrapeTT ct url = do
    cd <- return $ utctDay ct
    tts <- scrapeURL url timetables
    let (_, _, w) = toWeekDate cd
    return $ getTT tts w

mkURL :: String -> String -> String
mkURL code dest =
    "http://www.navi.kotsu.city.sendai.jp/dia/bustime/subway/subway_print.cgi" ++ params
    where
        params = "?PrintCode=" ++ code ++ "&PrintDest=" ++ dest

timetables :: Scraper String [RawTimeTable]
timetables = chroots ("table" @: [hasClass "timetable_p_s"] // "tr") tditem

tditem :: Scraper String RawTimeTable
tditem = do
    weekdaymins <- text $ "td" @: ["id" @= "timetable"]
    hour        <- text $ "td" @: ["id" @= "time"]
    weekendmins <- text $ "td" @: ["id" @= "timetable"]
    let weekdaymin = words $ replace "&nbsp;" " " weekdaymins
        weekendmin = words $ replace "&nbsp;" " " weekendmins
        hour' = replace " " "" $ replace "\n" "" $ hour
    return $ RawTimeTable weekdaymin hour' weekendmin

getTT :: Maybe [RawTimeTable] -> Int -> [TimeTable]
getTT Nothing _ = []
getTT (Just tts) day
    | day < 6   = map strWeekDayTT $ map (searchTTwithHr tts) $ [0..23]
    | otherwise = map strWeekEndTT $ map (searchTTwithHr tts) $ [0..23]

strWeekDayTT :: RawTimeTable -> TimeTable
strWeekDayTT (RawTimeTable ss h _) = TimeTable h ss

strWeekEndTT :: RawTimeTable -> TimeTable
strWeekEndTT (RawTimeTable _ h ss) = TimeTable h ss

strTThr :: RawTimeTable -> String
strTThr (RawTimeTable _ hr _) = replace " " "" $ replace "\n" "" hr

searchTTwithHr :: [RawTimeTable] -> Int -> RawTimeTable
searchTTwithHr [] _ = RawTimeTable [] "" []
searchTTwithHr (tt:ts) hr
    | strTThr tt == (show hr) = tt
    | otherwise = searchTTwithHr ts hr

getStaCode :: String -> StationData
getStaCode sta
    | sta =~ "仙台"           = StationData "9285" [("0", "富沢行"),
                                                ("1", "泉中央行"),
                                                ("2", "荒井行"),
                                                ("3", "八木山動物公園行")]
-- 南北線
    | sta =~ "泉中央"         = StationData "6646" [("0", "富沢行"), ("1", "泉中央行")]
    | sta =~ "八乙女"         = StationData "6647" [("0", "富沢行"), ("1", "泉中央行")]
    | sta =~ "黒松"           = StationData "6648" [("0", "富沢行"), ("1", "泉中央行")]
    | sta =~ "旭ケ丘"         = StationData "6649" [("0", "富沢行"), ("1", "泉中央行")]
    | sta =~ "台原"           = StationData "6650" [("0", "富沢行"), ("1", "泉中央行")]
    | sta =~ "北仙台"         = StationData "1222" [("0", "富沢行"), ("1", "泉中央行")]
    | sta =~ "北四番丁"       = StationData "6651" [("0", "富沢行"), ("1", "泉中央行")]
    | sta =~ "勾当台公園"     = StationData "6652" [("0", "富沢行"), ("1", "泉中央行")]
    | sta =~ "広瀬通"         = StationData "6653" [("0", "富沢行"), ("1", "泉中央行")]
    | sta =~ "五橋"           = StationData "6654" [("0", "富沢行"), ("1", "泉中央行")]
    | sta =~ "愛宕橋"         = StationData "6655" [("0", "富沢行"), ("1", "泉中央行")]
    | sta =~ "河原町"         = StationData "6656" [("0", "富沢行"), ("1", "泉中央行")]
    | sta =~ "長町一丁目"     = StationData "6657" [("0", "富沢行"), ("1", "泉中央行")]
    | sta =~ "長町"           = StationData "975"  [("0", "富沢行"), ("1", "泉中央行")]
    | sta =~ "長町南"         = StationData "6658" [("0", "富沢行"), ("1", "泉中央行")]
    | sta =~ "富沢"           = StationData "6659" [("0", "富沢行"), ("1", "泉中央行")]
-- 東西線
    | sta =~ "八木山動物公園"  = StationData "9727" [("2", "荒井行"), ("3", "八木山動物公園行")]
    | sta =~ "青葉山"         = StationData "9728" [("2", "荒井行"), ("3", "八木山動物公園行")]
    | sta =~ "川内"           = StationData "9729" [("2", "荒井行"), ("3", "八木山動物公園行")]
    | sta =~ "国際センター"   = StationData "9730" [("2", "荒井行"), ("3", "八木山動物公園行")]
    | sta =~ "大町西公園"     = StationData "9731" [("2", "荒井行"), ("3", "八木山動物公園行")]
    | sta =~ "青葉通一番町"   = StationData "9732" [("2", "荒井行"), ("3", "八木山動物公園行")]
    | sta =~ "宮城野通"       = StationData "9733" [("2", "荒井行"), ("3", "八木山動物公園行")]
    | sta =~ "連坊"           = StationData "9734" [("2", "荒井行"), ("3", "八木山動物公園行")]
    | sta =~ "薬師堂"         = StationData "9735" [("2", "荒井行"), ("3", "八木山動物公園行")]
    | sta =~ "卸町"           = StationData "9736" [("2", "荒井行"), ("3", "八木山動物公園行")]
    | sta =~ "六丁の目"       = StationData "9737" [("2", "荒井行"), ("3", "八木山動物公園行")]
    | sta =~ "荒井"           = StationData "9738" [("2", "荒井行"), ("3", "八木山動物公園行")]
    | otherwise               = StationData "" []

(=~) :: String -> String -> Bool
(=~) a b = or [a == b, a == b ++ "駅"]

