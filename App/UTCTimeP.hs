{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module App.UTCTimeP(UTCTimeP(..), readFormattedUTCTimeP, showFormattedUTCTimeP) where

import           Data.Text           as T
import           Data.Time
import Data.Aeson
import           Yesod.Core.Dispatch
import           Prelude
import Control.Applicative (pure)
import Data.Time.Clock.POSIX

newtype UTCTimeP = UTCTimeP { unUTCTimeP :: UTCTime } deriving (Read, Eq, Show)

readFormattedUTCTimeP :: Text -> UTCTimeP
readFormattedUTCTimeP = UTCTimeP . posixSecondsToUTCTime . fromInteger . read . T.unpack

showFormattedUTCTimeP :: UTCTimeP -> Text
showFormattedUTCTimeP = T.pack . show . toInteger' where
    toInteger' = round . utcTimeToPOSIXSeconds . unUTCTimeP :: UTCTimeP -> Integer

instance PathPiece UTCTimeP where
    toPathPiece = showFormattedUTCTimeP
    fromPathPiece = Just . readFormattedUTCTimeP

instance FromJSON UTCTimeP where
    parseJSON = withText "UTCTime" $ \x -> pure $ readFormattedUTCTimeP x

instance ToJSON UTCTimeP where
    toJSON = String . showFormattedUTCTimeP
