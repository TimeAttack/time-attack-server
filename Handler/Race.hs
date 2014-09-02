{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Race where

import Import

getRaceR :: Text -> Handler TypedContent
getRaceR title = do
    track <- runDB $ getBy $ UniqueMapTitle title
    case track of
        Nothing -> selectRep $ do provideRep $ return ("No such track" :: Text)
        Just(Entity _ thisMap) -> selectRep $ do provideRep $ return $ object [ "title" .= (mapTitle thisMap) ]
