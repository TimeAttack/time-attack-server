{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module App.Pieces (LatLngP(..), LatLngBoxP(..), VKToken(..), latLng) where

import           App.UTCTimeP        (UTCTimeP (..), readFormattedUTCTimeP,
                                      showFormattedUTCTimeP)
import           Data.Monoid         ((<>))
import           Data.Text           as T
import           Data.Time
import           Prelude
import           Yesod.Core.Dispatch

data LatLngBoxP = LatLngBoxP
    { sw :: LatLngP
    , ne :: LatLngP
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

instance PathPiece LatLngBoxP where
    toPathPiece (LatLngBoxP sw' ne') = join ";" [lat sw', lng sw', lat ne', lng ne'] where
        join sep l = T.intercalate sep (toText l) where
            toText = fmap (T.pack . show)
    fromPathPiece box = do
        let [swLat, swLng, neLat, neLng, _] = fmap (read . T.unpack) (T.splitOn ";" box)
        return $ LatLngBoxP (latLng swLat swLng) (latLng neLat neLng)

instance PathPiece LatLngP where
    toPathPiece coord = T.intercalate ";" args where
        args = Prelude.map T.pack [show (lat coord), show (lng coord)]
    fromPathPiece latLng' = Just $ latLng x y where
        (x:y:[]) =  Prelude.map (read . T.unpack) $ T.splitOn ";" latLng'

instance PathPiece VKToken where
    toPathPiece tkn = token tkn <> ":" <> showTime tkn where
        showTime = showFormattedUTCTimeP . UTCTimeP . expire
    fromPathPiece tknStr = Just $ VKToken x (readTime' y) where
        readTime' = unUTCTimeP . readFormattedUTCTimeP
        (x:y:[]) =  Prelude.map (read . T.unpack) $ T.splitOn "-" tknStr

