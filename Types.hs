{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module Types where

import           Common       	
import           Control.Applicative
import           Control.Lens
import           Control.Monad.State
import           Data.Aeson
import           Data.Aeson.TH
import           Data.ByteString
import           Data.Char
import 			 Data.Tree
import 			 Network.HTTP.Conduit

data Thing = Thing {
	  _thingId :: Maybe String
	, _thingName :: Maybe String
	, _thingKind :: String
	, _thingData_ :: Value
	}
	deriving (Show, Eq)

$(deriveJSON defaultOptions{fieldLabelModifier = thingFixer} ''Thing)
makeFields ''Thing

data Listing = Listing {
	  _listingBefore :: Maybe String
	, _listingAfter :: Maybe String
	, _listingModhash :: String
	, _listingChildren :: [Thing]
	} deriving (Show, Eq)

$(deriveJSON defaultOptions{fieldLabelModifier = listingFixer} ''Listing)
makeFields ''Listing

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

data Account = Account {
             _accountCreated          :: Integer
           , _accountCreatedUtc       :: Integer
           , _accountCommentKarma     :: Int
           , _accountHasMail          :: Maybe Bool
           , _accountHasModMail       :: Maybe Bool
           , _accountHasVerifiedEmail :: Bool
           , _accountIsFriend         :: Bool
           , _accountIsGold           :: Bool
           , _accountIsMod            :: Bool
           , _accountLinkKarma        :: Int
           , _accountModhash          :: Maybe String
           , _accountOver_18          :: Maybe Bool
           , _accountName             :: String
           }
    deriving (Show, Eq)

$(deriveJSON defaultOptions{fieldLabelModifier = accountFixer} ''Account)

makeFields ''Account

data Comment = Comment {
				  _commentApprovedBy :: Maybe String
				, _commentAuthor :: String
				, _commentAuthorFlairCssClass :: Maybe String
				, _commentAuthorFlairText :: Maybe String
				, _commentBannedBy :: Maybe String
				, _commentBody :: String
				, _commentBodyHtml :: String
				--, edited :: Maybe Integer
				, _commentGilded :: Int
				, _commentLikes  :: Maybe Bool
				, _commentLinkAuthor :: Maybe String
				, _commentLinkId :: String
				, _commentLinkTitle :: Maybe String
				, _commentLinkUrl :: Maybe String
				, _commentName :: String
				, _commentNumReports :: Maybe Int
				, _commentParentId :: String
				, _commentSaved :: Bool
				, _commentScoreHidden :: Bool
				, _commentSubreddit :: String
				, _commentSubredditId :: String
				, _commentDistinguished :: Maybe String
				, _commentUps :: Int
				, _commentDowns :: Int
				, _commentCreated :: Integer
				, _commentCreatedUtc :: Integer
				}
	deriving (Show, Eq)

$(deriveJSON defaultOptions{fieldLabelModifier = commentFixer} ''Comment)

makeFields ''Comment

data MoreChildren = MoreChildren {
	  _morechildrenCount :: Int
	, _morechildrenParentId :: String
	, _morechildrenChildren :: [String]
	, _morechildrenName :: String
	, _morechildrenId :: String
} deriving (Show, Eq)

$(deriveJSON defaultOptions{fieldLabelModifier = moreChildrenFixer} ''MoreChildren)
makeFields ''MoreChildren

data Link = Link {
				  _linkAuthor :: String
				, _linkAuthorFlairCssClass :: Maybe String
				, _linkAuthorFlairText :: Maybe String
				, _linkClicked :: Bool
				, _linkDomain :: String
				, _linkHidden :: Bool
				, _linkIsSelf :: Bool
				, _linkLikes  :: Maybe Bool
				, _linkLinkFlairCssClass :: Maybe String
				, _linkLinkFlairText :: Maybe String
				, _linkMedia :: Maybe Value
				, _linkMediaEmbed :: Maybe Value
				, _linkName :: String
				, _linkNumComments :: Int
				, _linkOver_18 :: Bool
				, _linkPermalink :: String
				, _linkSaved :: Bool
				, _linkScore :: Int
				, _linkSelftext :: Maybe String
				, _linkSelftextHtml :: Maybe String
				, _linkSubreddit :: String
				, _linkSubredditId :: String
				, _linkThumbnail :: String
				, _linkTitle :: String
				, _linkUrl :: String
				, _linkDistinguished :: Maybe String
				, _linkStickied :: Bool
				, _linkUps :: Int
				, _linkDowns :: Int
				, _linkCreated :: Integer
				, _linkCreatedUtc :: Integer
				}
	deriving (Show, Eq)

$(deriveJSON defaultOptions{fieldLabelModifier = linkFixer} ''Link)

makeFields ''Link

data RedditState = RedditState {
      _redditstateJar :: CookieJar
    , _redditstateModhash :: String
} deriving (Show, Eq)

makeFields ''RedditState

type Reddit = StateT RedditState IO

type Thread = Forest (Either MoreChildren Comment)

addAuth :: Request -> Reddit Request
addAuth req = do
    cs <- get
    return req {cookieJar = Just $ cs ^. jar}

fillRequest :: Request -> Reddit Request
fillRequest = addAuth . addUAString