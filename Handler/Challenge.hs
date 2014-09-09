{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Handler.Challenge(ChallengeR(..), postAttemptR) where

import           App.Pieces
import           Import
import Network.HTTP.Types.Status
import Data.Aeson.Types(withText)
import Data.Int(Int64)
import Data.Text as T

data ChallengeR = ChallengeR {
    userId:: Int64,
    trackId:: Int64,
    time:: UTCTimeP
} deriving (Eq, Show)

instance FromJSON UTCTimeP where
    --parseJSON a = trace (show a) $ fail "exc"
    parseJSON = withText "UTCTime" $ \x -> pure $ readFormattedUTCTimeP x

instance ToJSON UTCTimeP where
    toJSON = String . showFormattedUTCTimeP

instance ToJSON ChallengeR where
    toJSON c = object ["user-id" .= (userId c), "track-id" .= (trackId c), "time" .= (time c) ]

instance FromJSON ChallengeR where
    --parseJSON a = trace (show a) $ fail "exc"
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
    case (user, track) of
        (Just _, Just _) -> runDB $ insert $ Challenge userKey trackKey $ unUTCTimeP (time challenge)                                                     
        _ -> sendResponseStatus status400 ()
    sendResponseStatus status204 ()



