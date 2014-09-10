{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module App.Pieces where

import           Prelude
import           Data.Text           as T
import           Yesod.Core.Dispatch

data LatLngP = LatLngP {
        lat :: Double,
        lng :: Double
    } deriving (Eq, Show, Read)

instance PathPiece LatLngP where
    toPathPiece coord = T.intercalate "-" args where
        args = Prelude.map T.pack [show (lat coord), show (lng coord)]
    fromPathPiece latLng = Just $ LatLngP x y where
        (x:y:[]) =  Prelude.map (read . T.unpack) $ T.splitOn "-" latLng
