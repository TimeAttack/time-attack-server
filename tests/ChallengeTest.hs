{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module ChallengeTest
    ( challengeSpecs
    ) where

import           App.Pieces
import           Data.Aeson
import           Data.Text               as T
import           Data.Text.Lazy.Encoding
import           Data.Time
import           Handler.Challenge       (ChallengeR (..))
import           System.Locale
import           TestImport

challengeSpecs :: Spec
challengeSpecs =
    ydescribe "Challenge API" $ do
        let timeStr = "201412032333"
        let time = readTime defaultTimeLocale "%Y%m%d%M%S" timeStr
        yit "Let's submit a track with non existent user id. Should fail" $ do
            postBody AttemptR "{\"userId\" : 12, \"trackId\": 23, time: \"201412032333\"}"
            statusIs 400
        yit "Ok. Now Let's create a user and a temporary track. Then, submit again. Should pass" $ do
            trackKey <- runDB $ insert $ Track time (LatLng 100.2 100.3) [LatLng 23.1 22.45]
            userKey <- runDB $ insert $ User "name" (Just "pass")
            -- '@' doesn't work
            let Key(PersistInt64 trackId) = trackKey
            let Key(PersistInt64 userId) = userKey
            postBody AttemptR $ encode $ ChallengeR userId trackId (readFormattedUTCTimeP $ T.pack timeStr)
            printBody
            runDB $ delete trackKey
            runDB $ delete userKey
            statusIs 204
        yit "We've removed data from DB. It should be empty again" $ do
            postBody AttemptR "{\"userId\" : 12, \"trackId\": 23, time: \"201412032333\"}"
            statusIs 400



