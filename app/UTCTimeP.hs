{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module App.UTCTimeP(UTCTimeP(..), readFormattedUTCTimeP, showFormattedUTCTimeP) where

import           Data.Text           as T
import           Data.Time
import Data.Aeson
import           Yesod.Core.Dispatch
import           Prelude
import           System.Locale
import Control.Applicative (pure)

newtype UTCTimeP = UTCTimeP { unUTCTimeP :: UTCTime } deriving (Read, Eq, Show)

readFormattedUTCTimeP :: Text -> UTCTimeP
readFormattedUTCTimeP = UTCTimeP . (readTime defaultTimeLocale "%Y%m%d%M%S") . T.unpack

showFormattedUTCTimeP :: UTCTimeP -> Text
showFormattedUTCTimeP = T.pack . (formatTime defaultTimeLocale "%Y%m%d%M%S") . unUTCTimeP

instance PathPiece UTCTimeP where
    toPathPiece = showFormattedUTCTimeP
    fromPathPiece = Just . readFormattedUTCTimeP

instance FromJSON UTCTimeP where
    parseJSON = withText "UTCTime" $ \x -> pure $ readFormattedUTCTimeP x

instance ToJSON UTCTimeP where
    toJSON = String . showFormattedUTCTimeP
