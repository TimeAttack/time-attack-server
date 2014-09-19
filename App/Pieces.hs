{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module App.Pieces (LatLngP(..), LatLngBoxP(..), VKToken(..), latLng) where

import           Prelude
import           Data.Text           as T
import           Yesod.Core.Dispatch
import Data.Time
import           Data.Monoid ((<>))
import App.UTCTimeP(readFormattedUTCTimeP, showFormattedUTCTimeP, UTCTimeP(..))

data LatLngBoxP = LatLngBoxP
    { topRight :: LatLngP
    , bottomLeft :: LatLngP
    } deriving (Eq, Show, Read)

data LatLngP = LatLngP {
        lat :: Double,
        lng :: Double
    } deriving (Eq, Show, Read)

data VKToken = VKToken {token :: Text, expire :: UTCTime } deriving (Eq, Show)

latLng :: Double -> Double -> LatLngP
latLng lat' lng'
    | lat' < 85 && lat' > -85.05115 && lng' < 180 && lng' > -180 = LatLngP lat' lng'
    | otherwise = error "Invalid geographic coordinates. Out of bounds."

instance PathMultiPiece LatLngBoxP where
    toPathMultiPiece (LatLngBoxP a b) = [(toPathPiece a), (toPathPiece b)]
    fromPathMultiPiece (x:y:[]) = do
        topL <- fromPathPiece x
        botR <- fromPathPiece y
        return $ LatLngBoxP topL botR
    fromPathMultiPiece _ = Nothing

instance PathPiece LatLngP where
    toPathPiece coord = T.intercalate "-" args where
        args = Prelude.map T.pack [show (lat coord), show (lng coord)]
    fromPathPiece latLng' = Just $ latLng x y where
        (x:y:[]) =  Prelude.map (read . T.unpack) $ T.splitOn "-" latLng'

instance PathPiece VKToken where
    toPathPiece tkn = token tkn <> ":" <> showTime tkn where
        showTime = showFormattedUTCTimeP . UTCTimeP . expire
    fromPathPiece tknStr = Just $ VKToken x (readTime' y) where
        readTime' = unUTCTimeP . readFormattedUTCTimeP
        (x:y:[]) =  Prelude.map (read . T.unpack) $ T.splitOn "-" tknStr

