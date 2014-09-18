{-# LANGUAGE OverloadedStrings #-}
module TrackTest
    ( trackSpecs
    ) where

import           App.Pieces
import           App.UTCTimeP
import           Data.Aeson
import           Data.Time.Clock.POSIX
import           Handler.Track
import           TestImport

trackSpecs :: Spec
trackSpecs =
    ydescribe "Track API" $ do
        let timeStr = "1411034454"
        let time = (posixSecondsToUTCTime . fromInteger . read) timeStr
        yit "searches for track by created date. but there is no track" $ do
            get $ TrackR $ UTCTimeP time
            statusIs 404
            bodyContains timeStr
        yit "let's create new track, request it and remove it" $ do
            trackId <- runDB $ insert $ Track time (LatLng 1.2 1.3) [LatLng 23.1 22.45]
            get $ TrackR $ UTCTimeP time
            runDB (delete trackId)
            statusIs 200
        yit "DB contains no tracks again" $ do
            get $ TrackR $ UTCTimeP time
            statusIs 404
        yit "Lets try to submit new track with PUT and JSON" $ do
            request $ do
              setMethod "PUT"
              setUrl (TrackR $ UTCTimeP time)
              setRequestBody $ encode $ TrackP (UTCTimeP time) [LatLng 23.12 34.3, LatLng 45.3 43.23]
            statusIs 204
        yit "GET by ID should work now" $ do
            get $ TrackR $ UTCTimeP time
            statusIs 200
        yit "OK. Remove this track" $ do
            runDB (deleteBy $ UnicTrackDate time)
            get $ TrackR $ UTCTimeP time
            statusIs 404
        yit "Now lets find nearest tracks to our position. But there are no tracks in DB ;(" $ do
            get $ NearestTracksR $ LatLngP 10 10
            statusIs 404
        yit "Same story. Put track into DB. Search for it. And remove it" $ do
            trackId <- runDB $ insert $ Track time (LatLng 100.2 100.3) [LatLng 23.1 22.45]
            get $ NearestTracksR $ LatLngP 100 100
            runDB (delete trackId)
            statusIs 200
        yit "DB contains no tracks again" $ do
            get $ NearestTracksR $ LatLngP 100 100
            statusIs 404



