{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module RedditData.Common  where

import Data.Aeson
import Data.Aeson.TH
import Data.ByteString.Lazy
import Util (fieldFixer)

data Thing = Thing {
	id :: Maybe String
	, name :: Maybe String
	, kind :: String
	, data_ :: Value
	}
	deriving (Show, Eq)

data Listing = Listing {
	before :: Maybe String
	, after :: Maybe String
	, modhash :: String
	, children :: [Thing]
	} deriving (Show, Eq)

$(deriveJSON defaultOptions{fieldLabelModifier = fieldFixer} ''Thing)
$(deriveJSON defaultOptions{fieldLabelModifier = fieldFixer} ''Listing)