{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module ChallengeTest
    ( challengeSpecs
    ) where

import           App.UTCTimeP
import           Data.Aeson
import           Data.Int                (Int64)
import           Data.Text               as T
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Data.Time.Clock.POSIX
import           Handler.Challenge       (ChallengeR (..))
import           TestImport
import           Text.Shakespeare.Text

challengeSpecs :: Spec
challengeSpecs =
    ydescribe "Challenge API" $ do
        let timeStr = "1411034454"
        let time' = (posixSecondsToUTCTime . fromInteger . read) timeStr
        let timeInt64 = read timeStr :: Int64
        let checkpointsTime = [UTCTimeP time']
        yit "Let's submit a track with non existent user id. Should fail. No such track in DB" $ do
            postBody AttemptR (encodeUtf8 [lt|{"userId" : 12, "trackId": 23, "time": "#{timeStr}"}|])
            statusIs 400
        yit "Let's create a user and a temporary track. Then, submit again. Should pass" $ do
            trackKey <- runDB $ insert $ Track time' (LatLng 100.2 100.3) [LatLng 23.1 22.45]
            -- User will be here
            --userKey <- runDB $ insert $ User "name" (Just "pass")
            -- '@' doesn't work
            --let Key(PersistInt64 trackId') = trackKey
            --let Key(PersistInt64 userId') = userKey
            let userId' = 42 :: Int64
            let userIdInteger = fromIntegral userId' :: Int
            postBody AttemptR $ encode $ ChallengeR userId' timeInt64 checkpointsTime
            runDB $ deleteBy $ UnicUserTime (fromIntegral userIdInteger) time'
            runDB $ delete trackKey
            --runDB $ delete userKey
            statusIs 204
        yit "We've removed data from DB. It should be empty again" $ do
            postBody AttemptR (encodeUtf8 [lt|{"userId" : 12, "trackId": 23, "time": "#{timeStr}"}|])
            statusIs 400
        yit "Let's submit new challenge and get it by ID (user + time)" $ do
            trackKey <- runDB $ insert $ Track time' (LatLng 100.2 100.3) [LatLng 23.1 22.45]
            -- VK Auth disabled
            --userKey <- runDB $ insert $ User "name" (Just "pass")
            -- '@' doesn't work
            --let Key(PersistInt64 trackId') = trackKey
            --let Key(PersistInt64 userId') = userKey
            let userId' = 43
            let userIdInteger = fromIntegral userId' :: Int
            postBody AttemptR $ encode $ ChallengeR userId' timeInt64 checkpointsTime
            get $ ChallengeByIdR userIdInteger (UTCTimeP time')
            runDB $ deleteBy $ UnicUserTime (fromIntegral userIdInteger) time'
            runDB $ delete trackKey
            --runDB $ delete userKey
            statusIs 200
            bodyContains "23.1"



