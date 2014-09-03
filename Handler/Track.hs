{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Handler.Track where

import           App.Pieces
import           Import
import Data.Time
import           System.Locale

getTrackR :: UTCTimeP -> Handler Value
getTrackR date = runDB (getBy404 trackKey) >>= returnJson . fromTrack
    where
        trackKey = UnicTrackDate $ unUTCTimeP date        
        fromTrack :: Entity(Track) -> TrackResponse
        fromTrack (Entity _ track) = TrackResponse (trackCreated track) (trackCheckpoints track)    

data TrackResponse = TrackResponse 
    {   created :: UTCTime
      , checkpoints :: [LatLng]
    }

instance ToJSON TrackResponse where
 toJSON t =
    object [ "date"  .= formatTime defaultTimeLocale "%Y%m%d%M%S" (created t)
           , "checkpoints" .= (checkpoints t)
             ]    

