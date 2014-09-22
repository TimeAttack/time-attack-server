{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module App.UTCTimeP(UTCTimeP(..), readFormattedUTCTimeP, showFormattedUTCTimeP) where

import           Control.Applicative   (pure)
import           Data.Aeson
import           Data.Int
import           Data.Scientific       (Scientific, toBoundedInteger)
import           Data.Text             as T
import           Data.Time
import           Data.Time.Clock.POSIX
import           Prelude
import           Yesod.Core.Dispatch

newtype UTCTimeP = UTCTimeP { unUTCTimeP :: UTCTime } deriving (Read, Eq, Show, Ord)

class UTCTimePReadable a where
    readFormattedUTCTimeP :: a -> UTCTimeP

instance UTCTimePReadable Text where
    readFormattedUTCTimeP = UTCTimeP . posixSecondsToUTCTime . fromInteger . read . T.unpack

instance UTCTimePReadable Scientific where
    readFormattedUTCTimeP = (maybe (error "Not a number") fromInt64) . toBoundedInteger

fromInt64 :: Int64 -> UTCTimeP
fromInt64 = UTCTimeP . posixSecondsToUTCTime . fromIntegral

toInt :: UTCTimeP -> Integer
toInt = round . utcTimeToPOSIXSeconds . unUTCTimeP

showFormattedUTCTimeP :: UTCTimeP -> Text
showFormattedUTCTimeP = T.pack . show . toInt

instance PathPiece UTCTimeP where
    toPathPiece = showFormattedUTCTimeP
    fromPathPiece = Just . readFormattedUTCTimeP

instance FromJSON UTCTimeP where
    parseJSON = withScientific "UTCTimeP" $ \x -> pure $ readFormattedUTCTimeP x

instance ToJSON UTCTimeP where
    toJSON = Number . fromIntegral . toInt
