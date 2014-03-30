{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module RedditData (
    Account
    ) where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Char
import           Util          (fieldFixer, tagModifier)

data Account = Account {
             created          :: Integer
           , createdUtc       :: Integer
           , commentKarma     :: Int
           , hasMail          :: Maybe Bool
           , hasModMail       :: Maybe Bool
           , hasVerifiedEmail :: Bool
           , isFriend         :: Bool
           , isGold           :: Bool
           , isMod            :: Bool
           , linkKarma        :: Int
           , modhash          :: Maybe String
           , over_18          :: Maybe Bool
           , name             :: String
           }
    deriving (Show, Eq)



-- data ThingData = ThingData Int
--     deriving (Show, Eq)

$(deriveJSON defaultOptions{fieldLabelModifier = fieldFixer} ''Account)
