{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Race where

import Import

getRaceR :: Handler TypedContent
getRaceR = do
    track <- runDB $ getBy $ UniqueMapTitle "First"
    selectRep $ do
        provideRep $ return $ object [ "title" .= title ]
    where
      title = "a" :: Text
