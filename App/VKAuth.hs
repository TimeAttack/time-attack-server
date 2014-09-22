{-# LANGUAGE OverloadedStrings #-}
module App.VKAuth (vkCheckRequest, authVKClient) where

import           Data.Aeson                  (decode)
import           Data.Aeson.TH
import           Data.Char                   (toLower)
import           Data.Default                (def)
import           Data.IP
import           Data.List                   (find)
import qualified Data.Text.Encoding          as TE
import           Network.HTTP.Client.Conduit hiding (requestHeaders)
import qualified Network.HTTP.Conduit        as H
import           Network.HTTP.Types.Status
import           Network.Wai                 (requestHeaders)
import           Prelude
import           Yesod.Auth
import           Control.Monad               (mzero)
import           Control.Monad               (liftM)
import           Yesod.Core
import           Data.Text                   hiding (drop, find, toLower)

vkCheckRequest :: H.Request
vkCheckRequest = def {
       H.secure        = True
     , H.host          = TE.encodeUtf8 "api.vk.com"
     , H.port          = 443
     , H.path          = TE.encodeUtf8 ""
     , H.responseTimeout = Just 120000000 -- 2 minutes
     }

authVKClient :: YesodAuth m => AuthPlugin m
authVKClient = AuthPlugin
    { apName = "VK Client Token Auth"
    , apLogin = \_ -> toWidget [hamlet|"VK login"|]
    , apDispatch = \m ps ->
        case (m, ps) of
            ("POST", _) -> do
                  clientToken <- requireJsonBody
                  clientIp <- requireClientIp
                  manager <- liftM authHttpManager $ lift getYesod
                  tokenValidationRes <- checkVKToken (TokenCheck clientToken $ IP' clientIp) manager
                  case tokenValidationRes of
                    Just resp -> do
                        lift $ setCreds False $ Creds {
                              credsPlugin = "VK client auth"
                            , credsIdent = (pack . show . user_id) resp
                            , credsExtra = [("expire", expire resp)]}
                        sendResponseStatus status200 ("Success" :: Text)
                    Nothing -> sendResponseStatus status500 ("VK response failed" :: Text)
            (_, _) -> sendResponseStatus status500 ("Please, provide VK token" :: Text)
}

checkVKToken :: MonadIO m => TokenCheck -> H.Manager
        -> m(Maybe VKTokenResp)
checkVKToken tokenCheck mgr = do
  let body = [("token", (TE.encodeUtf8 . unToken . token) tokenCheck),
              ("ip", (TE.encodeUtf8 . pack . show . unIP' . ip) tokenCheck)]
  response <- liftIO $ H.httpLbs (H.urlEncodedBody body vkCheckRequest{path = "method/secure.checkToken", method = "POST"}) mgr
  return $ (decode (H.responseBody response) :: Maybe VKTokenResp)

requireClientIp :: (MonadHandler m) => m IP
requireClientIp = do
    request <- waiRequest
    let headers = requestHeaders request
    let forwardHeader = find (\(name, _) -> (name == "HTTP_X_FORWARDED_FOR")) headers
    case forwardHeader of
        Nothing -> sendResponseStatus status500 ("Can't resolve client IP" :: Text)
        Just (_, ip') -> return $ (read . unpack . TE.decodeUtf8) ip'

-- Prevent Orphan instance
newtype IP' = IP' { unIP' :: IP } deriving (Eq, Show)

data TokenCheck = TokenCheck {
  token :: VKToken,
  ip    :: IP'
} deriving (Eq, Show)

instance FromJSON IP' where
    parseJSON (String ip') = return $ IP' $ (read . unpack) ip'
    parseJSON _ = mzero

instance ToJSON IP' where
    toJSON = String . pack . show . unIP'

newtype VKToken = VKToken { unToken :: Text } deriving (Eq, Show)

data VKTokenResp = VKTokenResp { user_id :: Int, expire :: Text } deriving (Eq, Show)

$(deriveJSON defaultOptions{constructorTagModifier = fmap toLower, fieldLabelModifier = fmap toLower . drop 2 } ''VKToken)
$(deriveJSON defaultOptions ''TokenCheck)
$(deriveJSON defaultOptions ''VKTokenResp)
