{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import Types

import qualified Data.ByteString.Lazy            as L
import           Data.Text
import           Data.Text.IO                    (putStrLn)
import           Network.HTTP.Conduit            (httpLbs, parseUrl,
                                                  responseBody, urlEncodedBody,
                                                  withManager)
import           Network.OAuth.OAuth2
import           Network.OAuth.OAuth2.HttpClient

import Data.Aeson

import           Control.Applicative             ((<$>), (<*>))
import           Control.Monad                   (mzero)
import qualified Data.ByteString.Char8           as BS
import qualified Data.ByteString.Lazy.Internal   as BL
import           Data.Text                       (Text)
import           Network.HTTP.Types              (renderSimpleQuery)
import qualified Prelude                         as P (id)
import           System.Environment              (getArgs)


import           Prelude                         hiding (concat, putStrLn)
import qualified Text.HTML.DOM                   as DOM
import           Text.XML
import           Text.XML.Cursor

googleKey :: OAuth2
googleKey = OAuth2 { oauthClientId = "687600885182-mo2eamvhtmio6u8k4t5b9fave3rt35pd.apps.googleusercontent.com"
                   , oauthClientSecret = "e6zLqf-T5RPcA5t1j5752Ie0"
                   , oauthCallback = Just "urn:ietf:wg:oauth:2.0:oob"
                   , oauthOAuthorizeEndpoint = "https://accounts.google.com/o/oauth2/auth"
                   , oauthAccessTokenEndpoint = "https://accounts.google.com/o/oauth2/token"
                   }

main :: IO ()
main = do
    xs <- getArgs
    doGetInfo

doGetInfo :: IO ()
doGetInfo = do
    BS.putStrLn $ authorizationUrl googleKey `appendQueryParam` (googleScopeCalendar)
    putStrLn "visit the url and paste code here: "
    code <- fmap BS.pack getLine
    (Right token) <- fetchAccessToken googleKey code
    print token
    (getCalendarLists token :: IO (OAuth2Result CalendarList)) >>= print

googleScopeCalendar :: QueryParams
googleScopeCalendar = [("scope", "https://www.googleapis.com/auth/calendar")]

-- | Access offline
googleAccessOffline :: QueryParams
googleAccessOffline = [("access_type", "offline")
                      ,("approval_prompt", "force")]

gCal = "https://www.googleapis.com/calendar/v3/"
withGCal a = BS.pack (gCal ++ a)

{-getCalendarLists :: FromJSON a => AccessToken -> IO (OAuth2Result a)-}
getCalendarLists token = authGetJSON token $ "https://www.googleapis.com/calendar/v3/users/me/calendarList"

data Lesson = Lesson { startTime :: Text
                     , endTime   :: Text
                     , week      :: Text
                     , place     :: Text
                     , title     :: Text
                     , teacher   :: Text} deriving Show

lesson a b c d n x = Lesson {startTime = a, endTime = b, week = c, place = d, title = n, teacher = x}

toSchedule :: [Text] -> [(Text, [Lesson])] -> [(Text, [Lesson])]
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

{-main :: IO ()-}
{-main = do-}
    {-initReq <- parseUrl "http://www.ifmo.ru/module/isu_schedule.php"-}
    {-let req' = initReq-}
    {-let req = (flip urlEncodedBody) req' $-}
                {-[ ("group", "3538"),-}
                {-("week", "0") ]-}
    {-response <- withManager $ httpLbs req-}
    {-let page = responseBody response-}
    {-cursor <- cursorFor page-}
    {-let res = Prelude.filter (/= pack ": лек+пр.") $ cursor $// findNodes &// content-}
    {-let a = toSchedule res []-}
    {-mapM_ (putStrLn . pack . show) a-}
