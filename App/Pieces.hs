{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module App.Pieces where

import           Prelude
import           Data.Text           as T
import           Yesod.Core.Dispatch
import Data.Time
import Data.Time.Clock.POSIX
import           Data.Monoid ((<>))

data LatLngP = LatLngP {
        lat :: Double,
        lng :: Double
    } deriving (Eq, Show, Read)

instance PathPiece LatLngP where
    toPathPiece coord = T.intercalate "-" args where
        args = Prelude.map T.pack [show (lat coord), show (lng coord)]
    fromPathPiece latLng = Just $ LatLngP x y where
        (x:y:[]) =  Prelude.map (read . T.unpack) $ T.splitOn "-" latLng

data VKToken = VKToken {token :: Text, expire :: UTCTime } deriving (Eq, Show)

instance PathPiece VKToken where
    toPathPiece tkn = token tkn <> ":" <> expireTime where
        expireTime = (T.pack . show . utcTimeToPOSIXSeconds . expire) tkn
    fromPathPiece tknStr = Just $ VKToken x (expireTime y) where
        (x:y:[]) =  Prelude.map (read . T.unpack) $ T.splitOn "-" tknStr
        expireTime = posixSecondsToUTCTime . fromInteger . read . T.unpack
