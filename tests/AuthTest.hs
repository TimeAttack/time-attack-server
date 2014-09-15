{-# LANGUAGE OverloadedStrings #-}
module AuthTest where

import           TestImport

authSpecs :: Spec
authSpecs =
    ydescribe "VK authentication" $ do
        yit "Should accept random token and IP. Validation should failed." $ do
            --postBody (AuthR LoginR) "{\"ip\" : \"12.3.13.167\", \"token\": \"as2342sdq\"}"
            printBody
            statusIs 500



