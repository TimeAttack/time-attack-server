{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module ChallengeTest
    ( challengeSpecs
    ) where

import           Data.Aeson
import           Data.Text               as T
import           Handler.Challenge       (ChallengeR (..))
import Data.Time.Clock.POSIX
import Text.Shakespeare.Text
import Data.Text.Lazy.Encoding(encodeUtf8)
import           TestImport
import App.UTCTimeP

challengeSpecs :: Spec
challengeSpecs =
    ydescribe "Challenge API" $ do
        let timeStr = "1411034454"
        let time' = (posixSecondsToUTCTime . fromInteger . read) timeStr
        yit "Let's submit a track with non existent user id. Should fail. No such track in DB" $ do
            postBody AttemptR (encodeUtf8 [lt|{"userId" : 12, "trackId": 23, "time": "#{timeStr}"}|])
            statusIs 400
        yit "Ok. Now Let's create a user and a temporary track. Then, submit again. Should pass" $ do
            trackKey <- runDB $ insert $ Track time' (LatLng 100.2 100.3) [LatLng 23.1 22.45]
            userKey <- runDB $ insert $ User "name" (Just "pass")
            -- '@' doesn't work
            let Key(PersistInt64 trackId') = trackKey
            let Key(PersistInt64 userId') = userKey
            postBody AttemptR $ encode $ ChallengeR userId' trackId' (readFormattedUTCTimeP $ T.pack timeStr)
            runDB $ delete trackKey
            runDB $ delete userKey
            statusIs 204
        yit "We've removed data from DB. It should be empty again" $ do
            postBody AttemptR (encodeUtf8 [lt|{"userId" : 12, "trackId": 23, "time": "#{timeStr}"}|])
            statusIs 400
        yit "Let's submit new challenge and get it by ID (user + time)" $ do
            trackKey <- runDB $ insert $ Track time' (LatLng 100.2 100.3) [LatLng 23.1 22.45]
            userKey <- runDB $ insert $ User "name" (Just "pass")
            -- '@' doesn't work
            let Key(PersistInt64 trackId') = trackKey
            let Key(PersistInt64 userId') = userKey
            let userIdInteger = fromIntegral userId' :: Int
            postBody AttemptR $ encode $ ChallengeR userId' trackId' (readFormattedUTCTimeP $ T.pack timeStr)
            get $ ChallengeByIdR userIdInteger (UTCTimeP time')
            runDB $ deleteBy $ UnicUserTime (fromIntegral userIdInteger) time'
            runDB $ delete trackKey
            runDB $ delete userKey
            statusIs 200
            bodyContains "23.1"



