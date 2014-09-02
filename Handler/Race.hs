{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Race where

import Import

getRaceR :: Handler TypedContent
getRaceR = do
    track <- runDB $ getBy $ UniqueMapTitle "First"
    case track of
        Nothing -> selectRep $ do provideRep $ return ("No such track" :: Text)
        Just(Entity _ thisMap) -> selectRep $ do provideRep $ return $ object [ "title" .= (mapTitle thisMap) ]
