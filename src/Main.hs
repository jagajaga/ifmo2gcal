{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import qualified Data.ByteString.Lazy as L
import           Data.Text
import           Data.Text.IO         (putStrLn)
import           Network.HTTP.Conduit (httpLbs, parseUrl, responseBody,
                                       urlEncodedBody, withManager)
import           Network.OAuth.OAuth2
import           Network.OAuth.OAuth2.HttpClient

import           Control.Applicative             ((<$>), (<*>))
import           Control.Monad                   (mzero)
import           Data.Aeson                      (FromJSON, Value (Object),
                                                  parseJSON, (.:), (.:?))
import           Data.Aeson.TH                   (deriveJSON, defaultOptions)
import qualified Data.ByteString.Char8           as BS
import qualified Data.ByteString.Lazy.Internal   as BL
import           Data.Text                       (Text)
import           Network.HTTP.Types              (renderSimpleQuery)
import qualified Prelude                         as P (id)
import           System.Environment              (getArgs)


import           Prelude              hiding (concat, putStrLn)
import qualified Text.HTML.DOM        as DOM
import           Text.XML
import           Text.XML.Cursor

-- | oauthCallback = Just "https://developers.google.com/oauthplayground"
googleKey :: OAuth2
googleKey = OAuth2 { oauthClientId = "687600885182-mo2eamvhtmio6u8k4t5b9fave3rt35pd.apps.googleusercontent.com"
                   , oauthClientSecret = "e6zLqf-T5RPcA5t1j5752Ie0"
                   {-, oauthCallback = Just "https://developers.google.com/oauthplayground"-}
                   , oauthCallback = Just "urn:ietf:wg:oauth:2.0:oob"
                   , oauthOAuthorizeEndpoint = "https://accounts.google.com/o/oauth2/auth"
                   , oauthAccessTokenEndpoint = "https://accounts.google.com/o/oauth2/token"
                   }


--------------------------------------------------

data Token = Token { issued_to      :: Text
                   , audience       :: Text
                   , user_id        :: Maybe Text
                   , scope          :: Text
                   , expires_in     :: Integer
                   -- , email          :: Maybe Text
                   -- , verified_email :: Maybe Bool
                   , access_type    :: Text
                   } deriving (Show)


$(deriveJSON defaultOptions ''Token)

data User = User { id          :: Text
                 , name        :: Text
                 , given_name  :: Text
                 , family_name :: Text
                 , link        :: Text
                 , picture     :: Text
                 , gender      :: Text
                 , birthday    :: Text
                 , locale      :: Text
                 } deriving (Show)

$(deriveJSON defaultOptions ''User)

--------------------------------------------------

main :: IO ()
main = do
    xs <- getArgs
    case xs of
        ["offline"] -> offlineCase
        _ -> normalCase

offlineCase :: IO ()
offlineCase = do
    BS.putStrLn $ authorizationUrl googleKey `appendQueryParam` (googleScopeEmail ++ googleAccessOffline)
    putStrLn "visit the url and paste code here: "
    code <- fmap BS.pack getLine
    (Right token) <- fetchAccessToken googleKey code
    f token
    --
    -- obtain a new access token with refresh token, which turns out only in response at first time.
    -- Revoke Access https://www.google.com/settings/security
    --
    case refreshToken token of
        Nothing -> putStrLn "Failed to fetch refresh token"
        Just tk -> do
            (Right token) <- fetchRefreshToken googleKey tk
            f token
            --validateToken accessToken >>= print
            --(validateToken' accessToken :: IO (OAuth2Result Token)) >>= print
    where f token = do
            print token
            validateToken token >>= print
            (validateToken' token :: IO (OAuth2Result Token)) >>= print

normalCase :: IO ()
normalCase = do
    BS.putStrLn $ authorizationUrl googleKey `appendQueryParam` googleScopeUserInfo
    putStrLn "visit the url and paste code here: "
    code <- fmap BS.pack getLine
    (Right token) <- fetchAccessToken googleKey code
    putStr "AccessToken: " >> print token
    -- get response in ByteString
    validateToken token >>= print
    -- get response in JSON
    (validateToken' token :: IO (OAuth2Result Token)) >>= print
    -- get response in ByteString
    userinfo token >>= print
    -- get response in JSON
    (userinfo' token :: IO (OAuth2Result User)) >>= print

--------------------------------------------------
-- Google API

-- | This is special for google Gain read-only access to the user's email address.
googleScopeEmail :: QueryParams
googleScopeEmail = [("scope", "https://www.googleapis.com/auth/userinfo.email")]

-- | Gain read-only access to basic profile information, including a
googleScopeUserInfo :: QueryParams
googleScopeUserInfo = [("scope", "https://www.googleapis.com/auth/userinfo.profile")]

-- | Access offline
googleAccessOffline :: QueryParams
googleAccessOffline = [("access_type", "offline")
                      ,("approval_prompt", "force")]

-- | Token Validation
validateToken :: AccessToken -> IO (OAuth2Result BL.ByteString)
validateToken token = authGetBS token "https://www.googleapis.com/oauth2/v1/tokeninfo"

validateToken' :: FromJSON a => AccessToken -> IO (OAuth2Result a)
validateToken' token = authGetJSON token "https://www.googleapis.com/oauth2/v1/tokeninfo"

-- | fetch user email.
--   for more information, please check the playround site.
--
userinfo :: AccessToken -> IO (OAuth2Result BL.ByteString)
userinfo token = authGetBS token "https://www.googleapis.com/oauth2/v2/userinfo"

userinfo' :: FromJSON a => AccessToken -> IO (OAuth2Result a)
userinfo' token = authGetJSON token "https://www.googleapis.com/oauth2/v2/userinfo"


data Lesson = Lesson { startTime :: Text
                     , endTime   :: Text
                     , week      :: Text
                     , place     :: Text
                     , title      :: Text
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
