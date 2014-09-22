{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE PatternGuards #-}
module Handler.Challenge(ChallengeR(..), postAttemptR, getChallengeByIdR) where

import           Import
import Network.HTTP.Types.Status
import Data.Int(Int64)
import App.UTCTimeP(UTCTimeP(..))
import Data.List(head)
import           Data.Time.Clock.POSIX

getChallengeByIdR :: Int -> UTCTimeP -> Handler TypedContent
getChallengeByIdR user time' = do
     (Entity _ challenge) <- runDB $ getBy404 $ UnicUserTime (fromIntegral user) (unUTCTimeP time')
     track <- runDB $ get404 $ (challengeTrack challenge)
     let Key (PersistInt64 trackId') = challengeTrack challenge
     let challengeR = (ChallengeR <$> (round . challengeUser) <*> (const trackId') <*> (map UTCTimeP) . challengeTime) challenge
     selectRep $ do
        provideRep $ returnJson $ ChallengeResponse challengeR (trackCheckpoints track)
        provideRep $ defaultLayout $ challengeWidget

challengeWidget :: Widget
challengeWidget = do
  setTitleI MsgChallengeInfo
  addStylesheet $ StaticR css_bootstrap_min_css
  addScriptRemote "//cdn.jsdelivr.net/underscorejs/1.7.0/underscore-min.js"
  addScriptRemote "//maps.googleapis.com/maps/api/js?key=AIzaSyAzkMb90k70kJUzgFmzk5xMbbtA7ZvT9g0&sensor=false"
  $(widgetFile "challenge")

postAttemptR :: Handler Value
postAttemptR = do
    challenge <- requireJsonBody
    --let userKey = Key $ PersistInt64 (fromIntegral $ userId challenge)
    let userKey = (fromInteger . toInteger . userId) challenge :: Double
    (user, track) <- runDB $ do
        --user <- get userKey
        let user = Just userKey
        track <- getBy $ (UnicTrackDate . posixSecondsToUTCTime . fromIntegral . trackId) challenge
        return (user, track)
    _ <- case (user, track) of
            (Just _, Just(Entity trackKey trackData))
                | isMonotonousTimeSeq (time challenge)
                , sameLength (trackCheckpoints trackData) (time challenge)
                ->runDB $ insert $ (Challenge userKey trackKey <$> unUTCTimeP . head . time <*> (map unUTCTimeP) . time) challenge
            _ -> sendResponseStatus status400 ()
    sendResponseStatus status204 ()
    where
        isMonotonousTimeSeq :: [UTCTimeP] -> Bool
        isMonotonousTimeSeq (a:b:xs) | (a > b) = False
                                     | otherwise = isMonotonousTimeSeq (b:xs)
        isMonotonousTimeSeq (_:[]) = True
        isMonotonousTimeSeq [] = True
        sameLength = \x y -> (length x) == (length y)

data ChallengeR = ChallengeR {
    userId:: Int64,
    trackId:: Int64,
    time:: [UTCTimeP]
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





