{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module RedditData.Comment (
    Comment
    ) where

import           Common        (fieldFixer, tagModifier)
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Char

data Comment = Comment {
				  approvedBy :: Maybe String
				, author :: String
				, authorFlairCssClass :: Maybe String
				, authorFlairText :: Maybe String
				, bannedBy :: Maybe String
				, body :: String
				, bodyHtml :: String
				--, edited :: Maybe Integer
				, gilded :: Int
				, likes  :: Maybe Bool
				, linkAuthor :: Maybe String
				, linkId :: String
				, linkTitle :: Maybe String
				, linkUrl :: Maybe String
				, numReports :: Maybe Int
				, parentId :: String
				, saved :: Bool
				, scoreHidden :: Bool
				, subreddit :: String
				, subredditId :: String
				, distinguished :: Maybe String
				, ups :: Int
				, downs :: Int
				, created :: Integer
				, createdUtc :: Integer
				}
	deriving (Show, Eq)

$(deriveJSON defaultOptions{fieldLabelModifier = fieldFixer} ''Comment)
