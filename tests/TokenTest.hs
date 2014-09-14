{-# LANGUAGE OverloadedStrings #-}
module TokenTest
    ( tokenSpecs
    ) where

import           TestImport

tokenSpecs :: Spec
tokenSpecs =
    ydescribe "Token API" $ do
        yit "Should accept random token and IP. Validation should failed." $ do
            postBody TokenR "{\"ip\" : \"12.3.13.167\", \"token\": \"as2342sdq\"}"
            printBody
            statusIs 500



