{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Types where
import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Text            (Text)
import           GHC.Generics
import           Network.OAuth.OAuth2

data Token = Token { issued_to   :: Text
                   , audience    :: Text
                   , user_id     :: Maybe Text
                   , scope       :: Text
                   , expires_in  :: Integer
                   , access_type :: Text
                   } deriving (Show, Generic)
instance FromJSON Token
instance ToJSON Token

data CalendarList = CalendarList { id              :: Maybe Text
                                 , summary         :: Text
                                 , description     :: Maybe Text
                                 , location        :: Maybe Text
                                 , timeZone        :: Maybe Text
                                 , summaryOverride :: Maybe Text
                                 , hidden          :: Maybe Bool
                                 , selected        :: Maybe Bool
                                 , primary         :: Maybe Bool
                                 } deriving (Show, Generic)
instance FromJSON CalendarList
instance ToJSON CalendarList

data CalendarsList = CalendarsList { kind          :: Text
                                   , nextPageToken :: Maybe Text
                                   , items         :: [CalendarList]
                                   } deriving (Show, Generic)

instance FromJSON CalendarsList
instance ToJSON CalendarsList

googleKey :: OAuth2
googleKey = OAuth2 { oauthClientId = "687600885182-mo2eamvhtmio6u8k4t5b9fave3rt35pd.apps.googleusercontent.com"
                   , oauthClientSecret = "e6zLqf-T5RPcA5t1j5752Ie0"
                   , oauthCallback = Just "urn:ietf:wg:oauth:2.0:oob"
                   , oauthOAuthorizeEndpoint = "https://accounts.google.com/o/oauth2/auth"
                   , oauthAccessTokenEndpoint = "https://accounts.google.com/o/oauth2/token"
                   }


data Lesson = Lesson { startTime :: Text
                     , endTime   :: Text
                     , week      :: Text
                     , place     :: Text
                     , title     :: Text
                     , teacher   :: Text
                     } deriving Show

