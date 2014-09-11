{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TemplateHaskell   #-}
module Handler.Track(getTrackR, putTrackR, getNearestTracksR, TrackP(..)) where

import           App.Pieces
import           App.UTCTimeP
import           Import
import           Network.HTTP.Types.Status
import Data.Aeson.TH
import Data.Char(toLower)
import Data.List(head,last)

getTrackR :: UTCTimeP -> Handler Value
getTrackR date = runDB (getBy404 trackKey) >>= returnJson . fromTrack
    where
        trackKey = UnicTrackDate $ unUTCTimeP date

putTrackR :: UTCTimeP -> Handler Value
putTrackR time = do
  track <- requireJsonBody
  _ <- runDB $ insert $ Track (unUTCTimeP $ time) (center track) (checkpoints track)
  sendResponseStatus status204 ()

getNearestTracksR :: LatLngP -> Handler Value
getNearestTracksR latLng = do
  trackEntities <- runDB $ selectList([ TrackCenter >. lowerBound, TrackCenter <. upperBound]) []
  let tracks = map fromTrack trackEntities
  case tracks of [] -> notFound
                 _ -> returnJson tracks
  where
    range = 100
    lowerBound = LatLng ((lat latLng) - range) (lng latLng - range)
    upperBound = LatLng (lat latLng + range) ((lng latLng) + range)

data TrackP = TrackP
    {   created     :: UTCTimeP
      , checkpoints :: [LatLng]
    } deriving (Show)

center :: TrackP -> LatLng
center track = LatLng (sumLat / 2) (sumLng / 2) where
      firstPoint = head $ checkpoints track
      lastPoint = last $ checkpoints track
      sumLat = (latLngLat firstPoint) + (latLngLat lastPoint)
      sumLng = (latLngLng firstPoint) + (latLngLng lastPoint)

fromTrack :: Entity(Track) -> TrackP
fromTrack (Entity _ track) = TrackP (UTCTimeP $ trackCreated track) (trackCheckpoints track)

$(deriveJSON defaultOptions ''TrackP)
