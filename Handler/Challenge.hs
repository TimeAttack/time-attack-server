{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Handler.Challenge(ChallengeR(..), postAttemptR) where

import           Import
import Network.HTTP.Types.Status
import Data.Int(Int64)
import App.UTCTimeP

data ChallengeR = ChallengeR {
    userId:: Int64,
    trackId:: Int64,
    time:: UTCTimeP
} deriving (Eq, Show)

instance ToJSON ChallengeR where
    toJSON c = object ["user-id" .= userId c, "track-id" .= trackId c, "time" .= time c ]

instance FromJSON ChallengeR where
    parseJSON (Object o) =  ChallengeR
        <$> o .: "user-id"
        <*> o .: "track-id"
        <*> o .: "time"
    parseJSON a = fail $ "Challenge format error" ++ show a

postAttemptR :: Handler Value
postAttemptR = do
    challenge <- requireJsonBody
    let userKey = Key $ PersistInt64 (fromIntegral $ userId challenge)
    let trackKey = Key $ PersistInt64 (fromIntegral $ trackId challenge)
    (user, track) <- runDB $ do
        user <- get userKey
        track <- get trackKey
        return (user, track)
    _ <- case (user, track) of
            (Just _, Just _) -> runDB $ insert $ Challenge userKey trackKey $ unUTCTimeP (time challenge)
            _ -> sendResponseStatus status400 ()
    sendResponseStatus status204 ()




