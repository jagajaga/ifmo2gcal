{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Types where
import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Text           (Text)
import           GHC.Generics

--- TODO create normal json data to parse


data Token = Token { issued_to   :: Text
                   , audience    :: Text
                   , user_id     :: Maybe Text
                   , scope       :: Text
                   , expires_in  :: Integer
                   , access_type :: Text
                   } deriving (Show, Generic)
instance FromJSON Token 

data DefaultReminders = DefaultReminders { method  :: Text
                                         , minutes :: Integer
                                         } deriving (Show, Generic)
instance FromJSON DefaultReminders 
 
data CalendarList = CalendarList { kind             :: Text
                                 , etag             :: Text
                                 , id               :: Text
                                 , summary          :: Text
                                 , description      :: Text
                                 , location         :: Text
                                 , timeZone         :: Text
                                 , summaryOverride  :: Text
                                 , colorId          :: Text
                                 , backgroundColor  :: Text
                                 , foregroundColor  :: Text
                                 , hidden           :: Bool
                                 , selected         :: Bool
                                 , accessRole       :: Text
                                 , defaultReminders :: [DefaultReminders]
                                 , primary          :: Bool
                                 } deriving (Show, Generic)
instance FromJSON CalendarList

data CalendarsList = CalendarsList { cLkind :: Text
                                   , cLetag :: Text
                                   , cLnextPageToken :: Text
                                   , cLitems :: [CalendarList]
                                   } deriving (Show, Generic)

instance FromJSON CalendarsList
