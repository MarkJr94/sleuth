{-# LANGUAGE OverloadedStrings #-}

module Thread where

import           Control.Applicative
import 			 Control.Exception
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

--type Thread = 
thread :: Link -> Reddit (Forest (Either MoreChildren Comment))
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

func :: Value -> (Either MoreChildren Comment, [Value])
func v = (me, kids) where
			com' :: Maybe (Either String Comment)
			com' = fmap (parseEither parseJSON) (v ^? key "data")
			more' :: Maybe (Either String MoreChildren)
			more' = fmap (parseEither parseJSON) (v ^? key "data")
			kids' = do
				l <- v ^? key "data" . key "replies" . key "data" . key "children" . _Array
				Just $ V.toList l
			me = case (com', more') of
					(Just c, Just m) -> chooser m c
					(_, _) -> throw $ userError "Some sort of decoding error"
			kids = case kids' of
					Just k -> k
					Nothing -> []


chooser :: (Show a, Show c) => Either a b -> Either c d -> Either b d
chooser x y = case x of
	Right b' -> Left b'
	Left a' -> case y of
		Right d' -> Right d'
		Left c' -> error $ "Neither Either is Right in chooser: " ++ (show a') ++ "\n" ++ (show c')