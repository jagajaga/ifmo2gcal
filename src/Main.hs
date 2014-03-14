{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Types

import qualified Data.ByteString.Lazy            as L
import           Data.Text
import           Data.Text.Encoding
import           Data.Text.IO                    (putStrLn)
import           Network.HTTP.Conduit            (httpLbs, parseUrl,
                                                  responseBody, urlEncodedBody,
                                                  withManager)
import           Network.OAuth.OAuth2
import           Network.OAuth.OAuth2.HttpClient

import           Network.HTTP.Types              (hContentType)

import           Data.Aeson

import qualified Data.ByteString.Char8           as BS
import qualified Prelude                         as P (id)
import           System.Environment              (getArgs)


import           Prelude                         hiding (concat, putStrLn)
import qualified Text.HTML.DOM                   as DOM
import           Text.XML
import           Text.XML.Cursor

import           Data.Time
import           Data.Time.Calendar
import           Data.Time.Calendar.WeekDate

main :: IO ()
main = do
    (ifmoCal, token) <- doGetInfo
    let (Just calendarId) = Types.id ifmoCal
    schedule <- getSchedule
    date <- getCurrentTime >>= return . utctDay
    let (year, month, day') = toGregorian date
    let (curWeek, day) = (\(_, a, c) -> (a, day' - (c - 1))) $ toWeekDate date
    mapM_ (\(a, b) -> mapM (\el -> toCalendar token calendarId year (toInteger month)  (toInteger day) (toInteger curWeek) a el) b) schedule
    print "Success!"

toCalendar token calendarId year month day curWeek dayOfTheWeek a = authPostBSwithBody token (BS.pack ("https://www.googleapis.com/calendar/v3/calendars/" ++ unpack calendarId ++ "/events")) [] $ getRequest year (toInteger month)  (toInteger day) (toInteger curWeek) (toInteger dayOfTheWeek) (startTime a) (endTime a) (week a) (place a) (title a) (teacher a)

createEvent token = authPostJSONWithBody token "https://www.googleapis.com/calendar/v3/calendars" [] (BS.pack "{\n\"summary\": \"ifmoSchedule\"\n}")

getRequest year month day' curWeek dayOfTheWeek startT endT interval' location name orginizer = result
            where day = show $ ((+) dayOfTheWeek) $ if (((interval') == ("\1095\1077\1090")) && (even curWeek)) || (((interval') == ("\1085\1077\1095")) && (not $ even curWeek)) then day' else if (interval') == "\160" then day' else day' + 7
                  interval = show $ if  interval' == "\160" then 1 else 2
                  result = encodeUtf8 ( pack "{\n \"end\": {\n  \"dateTime\": \"" `append` (pack $ show year) `append` pack "-" `append` (pack $ show month) `append` pack "-" `append` pack day `append` pack "T" `append` endT `append` pack ":00\",\n  \"timeZone\": \"Europe/Moscow\"\n },\n \"start\": {\n  \"dateTime\": \""`append` (pack $ show year) `append` pack "-" `append` (pack $ show month) `append` pack "-" `append` pack day `append` pack "T" `append` startT `append` pack ":00\",\n  \"timeZone\": \"Europe/Moscow\"\n },\n \"recurrence\": [\n  \"RRULE:FREQ=WEEKLY;INTERVAL=" `append` pack interval `append` pack ";WKST=SU;UNTIL=20140627T000000Z\"\n ],\n \"location\": \"" `append` location `append` pack "\",\n \"organizer\": {\n  \"displayName\": \"" `append` orginizer `append` pack "\"\n },\n \"summary\": \"" `append` name `append` pack "\"\n}" )

getSchedule = do
    initReq <- parseUrl "http://www.ifmo.ru/module/isu_schedule.php"
    let req' = initReq
    let req = (flip urlEncodedBody) req' $
                [ ("group", "3538"),
                ("week", "0") ]
    response <- withManager $ httpLbs req
    let page = responseBody response
    cursor <- cursorFor page
    let res = Prelude.filter (/= pack ": лек+пр.") $ cursor $// findNodes &// content
    return $ toSchedule res []

doGetInfo = do
    BS.putStrLn $ authorizationUrl googleKey `appendQueryParam` (googleScopeCalendar)
    putStrLn "visit the url and paste code here: "
    code <- fmap BS.pack getLine
    (Right token) <- fetchAccessToken googleKey code
    (Right calendars) <- (getCalendarLists token :: IO (OAuth2Result CalendarsList))
    let cals = Prelude.filter (\a -> summary a == "ifmoSchedule") $ items calendars
    ifmoCal <- if Prelude.null cals then (createCalendar token :: IO (OAuth2Result CalendarList)) >>= (\(Right a) -> return a) else return $ Prelude.head cals
    return (ifmoCal, token)

createCalendar token = authPostJSONWithBody token "https://www.googleapis.com/calendar/v3/calendars" [] (BS.pack "{\n\"summary\": \"ifmoSchedule\"\n}")

validateToken token = authGetBS token "https://www.googleapis.com/oauth2/v1/tokeninfo"

googleScopeCalendar :: QueryParams
googleScopeCalendar = [("scope", "https://www.googleapis.com/auth/calendar")]

googleAccessOffline :: QueryParams
googleAccessOffline = [("access_type", "offline")
                      ,("approval_prompt", "force")]

getCalendarLists :: FromJSON a => AccessToken -> IO (OAuth2Result a)
getCalendarLists token = authGetJSON token $ "https://www.googleapis.com/calendar/v3/users/me/calendarList"

lesson a b c d n x = Lesson {startTime = a, endTime = b, week = c, place = d, title = n, teacher = x}

toDayNumber a = case a of
    "Понедельник" -> 0
    "Вторник" -> 1
    "Среда" -> 2
    "Четверг" -> 3
    "Пятница" -> 4
    "Суббота" -> 5

toSchedule :: [Text] -> [(Int, [Lesson])] -> [(Int, [Lesson])]
toSchedule (a:b:c:d:n:x:ss:xs) result = if a `elem` Prelude.map pack ["Понедельник", "Вторник", "Среда", "Четверг", "Пятница", "Суббота"]
    then toSchedule xs (result ++ [(toDayNumber a, [lesson b c d n x ss])])
    else toSchedule (ss:xs) result'
    where (curDay, lessons) = Prelude.last result
          result' = Prelude.init result ++ [(curDay, lessons ++ [lesson a b c d n x])]
toSchedule (a:b:c:d:n:x:[]) result = result'
    where (curDay, lessons) = Prelude.last result
          result' = Prelude.init result ++ [(curDay, lessons ++ [lesson a b c d n x])]
toSchedule ([]) res = error "FUCK YOU"

findNodes :: Cursor -> [Cursor]
findNodes = element "tr" &/ checkElement (\a -> let l = elementNodes a in
                                let ffst = Prelude.head l in
                                    case ffst of
                                        NodeElement a' -> elementName a' /= "strong"
                                        NodeContent _ -> True
                                        _ -> False)

cursorFor :: Monad m => L.ByteString -> m Cursor
cursorFor page = do
     return $ fromDocument $ DOM.parseLBS page
