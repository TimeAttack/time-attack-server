{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TemplateHaskell   #-}
module Handler.Challenge(ChallengeR(..), postAttemptR, getChallengeByIdR) where

import           Import
import Network.HTTP.Types.Status
import Data.Int(Int64)
import App.UTCTimeP(UTCTimeP(..))

getChallengeByIdR :: Int -> UTCTimeP -> Handler Value
getChallengeByIdR user time' = do
     (Entity _ challenge) <- runDB $ getBy404 $ UnicUserTime (fromIntegral user) (unUTCTimeP time')
     track <- runDB $ get404 $ (challengeTrack challenge)
     let Key (PersistInt64 trackId') = challengeTrack challenge
     let challengeR = (ChallengeR <$> (round . challengeUser) <*> (const trackId') <*> UTCTimeP . challengeTime) challenge
     returnJson $ ChallengeResponse challengeR (trackCheckpoints track)

postAttemptR :: Handler Value
postAttemptR = do
    challenge <- requireJsonBody
    --let userKey = Key $ PersistInt64 (fromIntegral $ userId challenge)
    let userKey = (fromInteger . toInteger . userId) challenge :: Double
    let trackKey = Key $ PersistInt64 (fromIntegral $ trackId challenge)
    (user, track) <- runDB $ do
        --user <- get userKey
        let user = Just userKey
        track <- get trackKey
        return (user, track)
    _ <- case (user, track) of
            (Just _, Just _) -> runDB $ insert $ Challenge userKey trackKey $ unUTCTimeP (time challenge)
            _ -> sendResponseStatus status400 ()
    sendResponseStatus status204 ()

data ChallengeR = ChallengeR {
    userId:: Int64,
    trackId:: Int64,
    time:: UTCTimeP
} deriving (Eq, Show)

data ChallengeResponse = ChallengeResponse ChallengeR [LatLng] deriving (Show)

instance ToJSON ChallengeResponse where
    toJSON (ChallengeResponse ch latlng) = object["challenge" .= ch, "map" .= latlng]

instance ToJSON ChallengeR where
    toJSON c = object ["user-id" .= userId c, "track-id" .= trackId c, "time" .= time c ]

instance FromJSON ChallengeR where
    parseJSON (Object o) =  ChallengeR
        <$> o .: "user-id"
        <*> o .: "track-id"
        <*> o .: "time"
    parseJSON a = fail $ "Challenge format error" ++ show a





