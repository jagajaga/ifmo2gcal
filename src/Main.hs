{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.HTTP.Conduit (simpleHttp, parseUrl, urlEncodedBody, withManager, httpLbs, responseBody)
import Prelude hiding (concat, putStrLn)
import Data.Text
import Data.Text.IO (putStrLn)
import qualified Data.ByteString.Lazy as L
import qualified Text.HTML.DOM as DOM
import Text.XML.Cursor
import Text.XML

import Debug.Trace

data Lesson = Lesson { startTime :: Text
                     , endTime :: Text
                     , week :: Text
                     , place :: Text
                     , name :: Text
                     , teacher :: Text} deriving Show
lesson a b c d n x = Lesson {startTime = a, endTime = b, week = c, place = d, name = n, teacher = x} 

toSchedule (a:b:c:d:n:x:ss:xs) result = if a `elem` Prelude.map pack ["Понедельник", "Вторник", "Среда", "Четверг", "Пятница", "Суббота"] 
    then toSchedule xs (result ++ [(a, [lesson b c d n x ss])]) 
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

main :: IO ()
main = do
    initReq <- parseUrl "http://www.ifmo.ru/module/isu_schedule.php"
    let req' = initReq
    let req = (flip urlEncodedBody) req' $
                [ ("group", "3538"),
                ("week", "0") ]
    response <- withManager $ httpLbs req
    let page = responseBody response
    cursor <- cursorFor page
    let res = Prelude.filter (/= pack ": лек+пр.") $ cursor $// findNodes &// content
    let a = toSchedule res []
    mapM_ (putStrLn . pack . show) a
