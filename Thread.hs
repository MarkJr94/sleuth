{-# LANGUAGE OverloadedStrings #-}

module Thread where

import           Control.Applicative
import 			 Control.Lens
import           Control.Monad.State
import           Data.Aeson
import           Data.Aeson.Types
import 			 Data.Aeson.Lens
import qualified Data.Conduit.List as DL
import Data.Conduit
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.UTF8 as LU
import qualified Data.ByteString.UTF8 as BU
import qualified Data.HashMap.Lazy as H
import           Data.List            (find)
import           Data.Maybe           (fromJust, fromMaybe)
import qualified Data.Text            as T
import           Data.Tree
import qualified Data.Vector as V
import Text.Printf (printf)
import           Network.HTTP.Conduit
import           Network.HTTP.Types

import           Auth
import           Types
import           Common

thread :: Link -> Reddit (Forest (Either String Comment))
thread link = do
	req' <- fmap addUAString (parseUrl $ "http://www.reddit.com" ++ (link ^. permalink) ++ ".json")
	cs <- get
	let req = req' {cookieJar = Just $ cs ^. jar }
	body <- fmap responseBody (withManager $ httpLbs req)
	let jsonRes :: Either String Value;
		jsonRes = eitherDecode body
	let tq = jsonRes ^? _Right . _Array . ix 1 . key "data" . key "children" . _Array
	let tq' = fromMaybe (error "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA") tq
	return $ (unfoldForest func . V.toList) tq'

func :: Value -> (Either String Comment, [Value])
func v = (me, kids) where
			me' = fmap (parseEither parseJSON) (v ^? key "data")
			kids' = do
				l <- v ^? key "data" . key "replies" . key "data" . key "children" . _Array
				Just $ V.toList l
			(me, kids) = case (me', kids') of
				(Just m, Just k) -> (m, k)
				(Just m, Nothing) -> (m, [])
				_ -> (Left "decoding error", [])