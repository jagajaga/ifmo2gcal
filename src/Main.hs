{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.HTTP.Conduit (simpleHttp, parseUrl, urlEncodedBody, withManager, httpLbs, responseBody)
import Prelude hiding (concat, putStrLn)
import Data.Text (concat)
import Data.Text.IO (putStrLn)
import qualified Data.ByteString.Lazy as L
import qualified Text.HTML.DOM as DOM
import Text.XML.Cursor
import Text.XML

import Debug.Trace

data Lesson = Lesson { startTime :: String
                     , endTime :: String
                     , week :: Bool
                     , place :: String
                     , name :: String
                     , teacher :: String}

-- The data we're going to search for
findNodes :: Cursor -> [Cursor]
findNodes = element "tr" &/ checkElement (\a -> let l = elementNodes a in 
                                let ffst = head l in
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
  {-L.putStrLn page-}
  cursor <- cursorFor page
  mapM_ putStrLn $ cursor $// findNodes &// content
