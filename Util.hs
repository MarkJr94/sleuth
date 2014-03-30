{-# LANGUAGE OverloadedStrings #-}

module Util(fieldFixer,
    tagModifier,
    userAgent,
    getAtMe) where

import           Data.Aeson
import qualified Data.ByteString     as B
import           Data.Char
import qualified Data.HashMap.Strict as HS
import qualified Data.Text           as T

fieldFixer :: String -> String
fieldFixer s = if s == "over_18"
	then s
	else toSnakeCase $ removeUnderscore s where
    	removeUnderscore = filter (/= '_')
        toSnakeCase = foldr (\c acc -> if isUpper c
        	then '_' : toLower c : acc
            else c : acc) ""

tagModifier :: String -> String
tagModifier s
    | s == "Account" = "t2"
    | otherwise = s

userAgent :: B.ByteString
userAgent = "https://github.com/MarkJr94/sleuth"

getAtMe :: Value -> Value
getAtMe (Object o) = case HS.lookup (T.pack "data") o of
	Just v -> v
	_ -> error "No data Brodo!"
getAtMe _ = error "No data Brodo!"