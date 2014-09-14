{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
module Handler.Token where

import           Data.Aeson                  (decode)
import           Data.Aeson.TH
import           Data.ByteString             (ByteString)
import           Data.Default                (def)
import           Data.List                   (find)
import qualified Data.Text.Encoding          as TE
import           Import
import           Network.HTTP.Client.Conduit hiding (requestHeaders)
import qualified Network.HTTP.Client.Conduit as H
import           Network.HTTP.Types.Status
import           Network.Wai                 (requestHeaders)
import Data.Char(toLower)


vkCheckRequest :: H.Request
vkCheckRequest = def {
       H.secure        = True
     , H.host          = TE.encodeUtf8 "vk.com"
     , H.port          = 443
     , H.path          = TE.encodeUtf8 "dev"
     , H.responseTimeout = Just 120000000 -- 2 minutes
     }

postTokenR :: Handler Value
postTokenR = do
  clientToken <- requireJsonBody
  clientIp <- requireClientIp
  let body = [("token", (TE.encodeUtf8 $ unToken clientToken)), ("ip", clientIp)]
  response <- httpLbs $ (H.urlEncodedBody body vkCheckRequest{method = "POST"})
  resValue <- return $ (decode (H.responseBody response) :: Maybe VKTokenResp)
  case resValue of
    Just _ -> sendResponseStatus status200 ("Success" :: Text)
    Nothing -> sendResponseStatus status500 ("VK response failed" :: Text)


requireClientIp :: (MonadHandler m) => m ByteString
requireClientIp = do
    request <- waiRequest
    let headers = requestHeaders request
    let forwardHeader = find (\(name, _) -> (name == "HTTP_X_FORWARDED_FOR")) headers
    case forwardHeader of
        Nothing -> sendResponseStatus status500 ("Can't resolve client IP" :: Text)
        Just (_, ip') -> return ip'


data TokenCheck = TokenCheck {
  token :: Token,
  ip    :: Text
} deriving (Eq, Show)

newtype Token = Token { unToken :: Text } deriving (Eq, Show)

data VKTokenResp = VKTokenResp { success :: Int, user_id :: Int, expire :: Text } deriving (Eq, Show)

$(deriveJSON defaultOptions{constructorTagModifier = fmap toLower, fieldLabelModifier = fmap toLower . drop 2 } ''Token)
$(deriveJSON defaultOptions ''TokenCheck)
$(deriveJSON defaultOptions ''VKTokenResp)
