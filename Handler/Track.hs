{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TemplateHaskell   #-}
module Handler.Track(getTrackByBoxR, getTrackR, putTrackR, getNearestTracksR, TrackP(..)) where

import           App.Pieces
import           App.UTCTimeP
import           Import
import           Network.HTTP.Types.Status
import Data.Aeson.TH
import Data.List(head)

getTrackR :: UTCTimeP -> Handler Value
getTrackR date = runDB (getBy404 trackKey) >>= returnJson . fromTrack
  where
      trackKey = UnicTrackDate $ unUTCTimeP date

putTrackR :: UTCTimeP -> Handler Value
putTrackR time = do
  track <- requireJsonBody
  _ <- runDB $ insert $ (Track (unUTCTimeP time) <$> center <*> checkpoints) track
  sendResponseStatus status204 ()
  where
    center :: TrackP -> LatLng
    center = LatLng <$> (/ 2) . sumLat <*> (/ 2) . sumLng where
    sumLat = (+) <$> latLngLat . topRightTrackPoint <*> latLngLat . bottomLeftTrackPoint
    sumLng = (+) <$> latLngLng . topRightTrackPoint <*> latLngLng . bottomLeftTrackPoint

getTrackByBoxR :: LatLngBoxP -> Handler Value
getTrackByBoxR box = searchInBox box >>= returnJson

getNearestTracksR :: LatLngP -> Handler Value
getNearestTracksR latLngP = searchInBox box >>= returnJson
  where
    range = 1
    topRight' = latLng (lat latLngP + range) (lng latLngP + range)
    bottomLeft' = latLng (lat latLngP - range) (lng latLngP - range)
    box = LatLngBoxP topRight' bottomLeft'

searchInBox :: LatLngBoxP -> HandlerT App IO [TrackP]
searchInBox box = do
  tracks <- runDB $ selectList([ TrackCenter >. (toLatLng $ bottomLeft box), TrackCenter <. (toLatLng $ topRight box)]) []
  return $ fmap fromTrack tracks
  where
    toLatLng :: LatLngP -> LatLng
    toLatLng = LatLng <$> lat <*> lng

data TrackP = TrackP
    {   created     :: UTCTimeP
      , checkpoints :: [LatLng]
    } deriving (Show)

topRightTrackPoint :: TrackP -> LatLng
topRightTrackPoint track = foldr topRightPartial initial (checkpoints track)
  where
    initial = (head . checkpoints) track
    topRightPartial = \x y -> LatLng (max (latLngLat x) (latLngLat y)) (max (latLngLng x) (latLngLng y))

bottomLeftTrackPoint :: TrackP -> LatLng
bottomLeftTrackPoint track = foldr bottomLeftPartial initial (checkpoints track)
  where
    initial = (head . checkpoints) track
    bottomLeftPartial = \x y -> LatLng (min (latLngLat x) (latLngLat y)) (min (latLngLng x) (latLngLng y))

fromTrack :: Entity(Track) -> TrackP
fromTrack (Entity _ track) = TrackP (UTCTimeP $ trackCreated track) (trackCheckpoints track)

$(deriveJSON defaultOptions ''TrackP)
