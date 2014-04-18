{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module RedditData.Link (
    Link
    ) where

import           Common        (fieldFixer, tagModifier)
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Char

data Link = Link {
				  author :: String
				, authorFlairCssClass :: Maybe String
				, authorFlairText :: Maybe String
				, clicked :: Bool
				, domain :: String
				, hidden :: Bool
				, isSelf :: Bool
				, likes  :: Maybe Bool
				, linkFlairCssClass :: Maybe String
				, linkFlairText :: Maybe String
				, media :: Maybe Value
				, mediaEmbed :: Maybe Value
				, numComments :: Int
				, over_18 :: Bool
				, permalink :: String
				, saved :: Bool
				, score :: Int
				, selftext :: Maybe String
				, selftextHtml :: Maybe String
				, subreddit :: String
				, subredditId :: String
				, thumbnail :: String
				, title :: String
				, url :: String
				, distinguished :: Maybe String
				, stickied :: Bool
				, ups :: Int
				, downs :: Int
				, created :: Integer
				, createdUtc :: Integer
				}
	deriving (Show, Eq)

$(deriveJSON defaultOptions{fieldLabelModifier = fieldFixer} ''Link)
