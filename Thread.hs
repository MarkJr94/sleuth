{-# LANGUAGE OverloadedStrings #-}

module Thread (
	  thread
	, fillThread
	, drawThread) where

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
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as B8
import qualified Data.HashMap.Lazy as H
import           Data.List            (find, intercalate, partition)
import           Data.Maybe           (fromJust, fromMaybe)
import qualified Data.Text            as T
import           Data.Tree
import qualified Data.Vector as V
import Text.Printf (printf)
import           Network.HTTP.Conduit
import           Network.HTTP.Types

import           Auth
import           Types
import qualified Types as Ty
import           Common

--type Thread = 
thread :: Link -> Reddit Thread
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

drawThread :: Thread -> String
drawThread th = drawForest $ map ((\x -> case x of
    Left mc ->  "[" ++ (show $ length $ mc ^. Types.children) ++ " MORE HIDDEN]"
    Right c -> c ^. author) <$>) th

fillThread :: Link -> Thread -> Reddit Thread
fillThread link f =  do
	comments <- mapM (getKids link) f
	let flatComments = concat comments
	return $ map (reparent flatComments) f

func :: Value -> (Either MoreChildren Comment, [Value])
func v = (me, kids) where
			v' = v ^? key "data"
			kids' = do
				l <- v ^? key "data" . key "replies" . key "data" . key "children" . _Array
				Just $ V.toList l
			me = getCommentOrKids v'
			kids = case kids' of
					Just k -> k
					Nothing -> []

getKids :: Link
    -> Tree (Either MoreChildren a)
    -> Reddit [Either MoreChildren Comment]
getKids link (Node (Left mc) ks) = (mangle) where
	mangle = do
		v <- moreKids link mc
		let arr = V.toList <$> (v ^? key "json" . key "data" . key "things" . _Array)
		let arr' =  (map (\x -> fromJust $ x ^? key "data")) $ fromJust arr
		let cs = map (getCommentOrKids . Just) arr'
		stuff <- mapM (getKids link) ks
		return $ cs  ++ (concat stuff)
getKids _ (Node (Right _) _) = do
	return []

moreKids :: Link -> MoreChildren -> Reddit Value
moreKids link mc = let bodyFunc :: Request -> Request;
					   bodyFunc = urlEncodedBody 
					   	[("link_id", B8.pack $ link ^. name),
					   		("api_type", "json"),
					   		("children", B8.pack $ intercalate "," (mc ^. Ty.children))]
   	in do
   		request <- fmap bodyFunc (parseUrl $ "http://www.reddit.com/api/morechildren/.json")
   		req' <- fillRequest request
   		resp <- withManager $ httpLbs request
   		let body = responseBody resp
   		--liftIO $ putStrLn $ L8.unpack body
   		let jsonRes = eitherDecode body
   		let v = eitherToException jsonRes
   		return v

getCommentOrKids :: Maybe Value -> Either MoreChildren Comment
getCommentOrKids v = me where
	com' :: Maybe (Either String Comment)
	com' = fmap (parseEither parseJSON) v
	more' :: Maybe (Either String MoreChildren)
	more' = fmap (parseEither parseJSON) v
	me = case (com', more') of
		(Just c, Just m) -> chooser m c
		(_, _) -> throw $ userError "Some sort of decoding error"	

chooser :: Either a b -> Either c d -> Either b d
chooser x y = case x of
	Right b' -> Left b'
	Left a' -> case y of
		Right d' -> Right d'
		Left c' -> error $ "Neither Either is Right in chooser"

reparent :: [Either MoreChildren Comment] 
	-> Tree (Either MoreChildren Comment)
	-> Tree (Either MoreChildren Comment)
reparent cs (Node r@(Right com) ks) = thing where
	(www, zzz) = partition (\c -> 
		case c of
			Left mc -> mc ^. parentId == com ^. name
			Right c' -> c' ^. parentId == com ^. name) cs
	www' = map (\x -> Node x []) www
	thing = Node r (www' ++ (map (reparent zzz) ks))
reparent _ n@(Node (Left _) _) = n