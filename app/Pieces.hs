{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module App.Pieces where

import           Data.Text           as T
import           Data.Time
import           Prelude
import           System.Locale
import           Yesod.Core.Dispatch

newtype UTCTimeP = UTCTimeP { unUTCTimeP :: UTCTime } deriving (Read, Eq, Show)

readFormattedUTCTimeP :: Text -> UTCTimeP
readFormattedUTCTimeP = UTCTimeP . (readTime defaultTimeLocale "%Y%m%d%M%S") . T.unpack

showFormattedUTCTimeP :: UTCTimeP -> Text
showFormattedUTCTimeP = T.pack . (formatTime defaultTimeLocale "%Y%m%d%M%S") . unUTCTimeP

instance PathPiece UTCTimeP where
    toPathPiece = showFormattedUTCTimeP
    fromPathPiece = Just . readFormattedUTCTimeP
 

data LatLngP = LatLngP {
        lat :: Double,
        lng :: Double
    } deriving (Eq, Show, Read) 

instance PathPiece LatLngP where  
    toPathPiece coord = T.intercalate "-" args where                 
        args = Prelude.map T.pack [show (lat coord), show (lng coord)]
    fromPathPiece latLng = Just $ LatLngP x y where
        (x:y:[]) =  Prelude.map (read . T.unpack) $ T.splitOn "-" latLng