{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module RedditData.Common  where

import           Common              (fieldFixer)
import           Control.Applicative
import           Control.Monad.State
import           Data.Aeson
import           Data.Aeson.TH
import           Data.ByteString

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

data Age = DefaultAge
	| Hour
	| Day
	| Week
	| Month
	| Year
	| AllTime

data Popularity = DefaultPop
	| Hot
	| New
	| Rising
	| Top
	| Controversial

formatAge :: Age -> ByteString
formatAge a = case a of
	DefaultAge -> ""
	Hour -> "hour"
	Day -> "day"
	Week -> "week"
	Month -> "month"
	Year -> "year"
	AllTime -> "all"

formatPop :: Popularity -> ByteString
formatPop p = case p of
	DefaultPop -> ""
	Hot -> "hot"
	New -> "new"
	Rising -> "rising"
	Top -> "top"
	Controversial -> "controversial"

type ListingState = StateT (Maybe Listing) IO


$(deriveJSON defaultOptions{fieldLabelModifier = fieldFixer} ''Thing)
$(deriveJSON defaultOptions{fieldLabelModifier = fieldFixer} ''Listing)
