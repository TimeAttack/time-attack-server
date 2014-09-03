{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module App.Pieces where

import           Data.Text           as T
import           Data.Time
import           Prelude
import           System.Locale
import           Yesod.Core.Dispatch

newtype UTCTimeP = UTCTimeP { unUTCTimeP :: UTCTime } deriving (Read, Eq, Show)
instance PathPiece UTCTimeP where
    toPathPiece time = T.pack $ formatTime defaultTimeLocale "%Y%m%d%M%S" (unUTCTimeP time)
    fromPathPiece x = Just (UTCTimeP $ readTime defaultTimeLocale "%Y%m%d%M%S" $ T.unpack x)
