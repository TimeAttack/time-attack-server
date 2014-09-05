{-# LANGUAGE OverloadedStrings #-}
module TrackTest
    ( trackSpecs
    ) where

import           Data.Time
import           System.Locale
import           TestImport
import App.Pieces

trackSpecs :: Spec
trackSpecs = 
    ydescribe "Track API" $ do 
        let timeStr = "201412032333"
        let time = readTime defaultTimeLocale "%Y%m%d%M%S" "201412032333"
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
        yit "Now lets find nearest tracks to our position. But there are no tracks in DB ;(" $ do
            get $ NearestTracksR $ LatLngP 10 10
            printBody
            statusIs 404
        yit "Same story. Put track into DB. Search for it. And remove it" $ do
            trackId <- runDB $ insert $ Track time (LatLng 100.2 100.3) [LatLng 23.1 22.45]            
            get $ NearestTracksR $ LatLngP 100 100
            runDB (delete trackId)    
            statusIs 200
        yit "DB contains no tracks again" $ do            
            get $ NearestTracksR $ LatLngP 100 100
            statusIs 404        



