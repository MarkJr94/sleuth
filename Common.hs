{-# LANGUAGE OverloadedStrings #-}

module Common(
    thingFixer,
    listingFixer,
    fieldFixer,
    accountFixer,
    linkFixer,
    commentFixer,
    moreChildrenFixer,
    userAgent,
    addUAString,
    resultToEither,
    eitherToException) where

import           Control.Exception
import           Data.Aeson
import qualified Data.ByteString      as B
import           Data.Char
import           Network.HTTP.Conduit
import           Network.HTTP.Types

fieldFixer :: String -> String
fieldFixer s = if s == "over_18"
    then s
    else toSnakeCase $ removeUnderscore s where
        removeUnderscore = filter (/= '_')
        toSnakeCase = foldr (\c acc -> if isUpper c
            then '_' : toLower c : acc
            else c : acc) ""

linkFixer :: String -> String
linkFixer = genericFixer 5

commentFixer :: String -> String
commentFixer = genericFixer 8

accountFixer :: String -> String
accountFixer = genericFixer 8

thingFixer :: String -> String
thingFixer = genericFixer 6

listingFixer :: String -> String
listingFixer = genericFixer 8

moreChildrenFixer :: String -> String
moreChildrenFixer = genericFixer 13

userAgent :: B.ByteString
userAgent = "https://github.com/MarkJr94/sleuth"

addUAString :: Request -> Request
addUAString request = request { requestHeaders = fixedHeaders } where
    oldHeaders = requestHeaders request
    fixedHeaders = (hUserAgent, userAgent)
        : filter (\(name, _) -> name /= hUserAgent) oldHeaders

resultToEither :: Result a -> Either String a
resultToEither x = case x of
    Success good -> Right good
    Error bad  -> Left bad

eitherToException :: Either String a -> a
eitherToException (Right x) = x
eitherToException (Left err) = throw $ userError err

-- ============================================================================
-- =========================== PRIVATE FUNCTIONS BEGIN ========================
-- ============================================================================

genericFixer :: Int -> String -> String
genericFixer n s = fieldFixer s' where
    base = drop n s
    newFirst = toLower $ head base
    s' = newFirst : tail base