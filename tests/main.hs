{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Import
import Yesod.Default.Config
import Yesod.Test
import Test.Hspec (hspec)
import Application (makeFoundation)

import HomeTest
import ChallengeTest
import TrackTest
--import AuthTest

main :: IO ()
main = do
    conf <- Yesod.Default.Config.loadConfig $ (configSettings Testing)
                { csParseExtra = parseExtra
                }
    foundation <- makeFoundation conf
    hspec $ do
        yesodSpec foundation $ do
            homeSpecs
            trackSpecs
            challengeSpecs
            --authSpecs
