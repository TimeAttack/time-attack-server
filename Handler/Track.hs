{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Handler.Track(getTrackR, getNearestTracksR) where

import           App.Pieces
import           Import
import Data.Time
import           System.Locale

getTrackR :: UTCTimeP -> Handler Value
getTrackR date = runDB (getBy404 trackKey) >>= returnJson . fromTrack
    where
        trackKey = UnicTrackDate $ unUTCTimeP date        

getNearestTracksR :: LatLngP -> Handler Value
getNearestTracksR latLng = runDB (
  selectList([ TrackCenter >. lowerBound, TrackCenter <. upperBound]) []
  ) >>= returnJson . (map fromTrack) where 
    range = 100
    lowerBound = LatLng ((lat latLng) + range) (lng latLng)
    upperBound = LatLng (lat latLng) ((lng latLng) + range)

data TrackResponse = TrackResponse 
    {   created :: UTCTime
      , checkpoints :: [LatLng]
    }

instance ToJSON TrackResponse where
 toJSON t =
    object [ "date"  .= formatTime defaultTimeLocale "%Y%m%d%M%S" (created t)
           , "checkpoints" .= (checkpoints t)
             ]    

fromTrack :: Entity(Track) -> TrackResponse
fromTrack (Entity _ track) = TrackResponse (trackCreated track) (trackCheckpoints track)      