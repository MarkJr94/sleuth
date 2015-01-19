{-# LANGUAGE OverloadedStrings #-}

module Thread (
	  thread
	, fillThread
	, drawThread
	, thing
	, garble) where

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
import           Data.Either (lefts, rights)
import qualified Data.HashMap.Lazy as H
import           Data.List            (find, intercalate, partition, sortBy, nub)
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
    Right c -> c ^. author ++ " " ++ c ^. name) <$>) th

fillThread :: Link -> Thread -> Reddit Thread
fillThread link f =  do
	comments <- mapM (getKids link) f
	let flatComments = concat comments
	liftIO $ print flatComments
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
		cs  <- moreKids link mc
		stuff <- mapM (getKids link) ks
		return $ cs  ++ (concat stuff)
getKids _ (Node (Right _) _) = do
	return []

moreKids :: Link -> MoreChildren -> Reddit [Either MoreChildren Comment]
moreKids link mc = do
	mk <- moreKidsReal link mc
	if (length mk) == 0
			then continue link mc
			else return mk

moreKidsReal :: Link -> MoreChildren -> Reddit [Either MoreChildren Comment]
moreKidsReal link mc = let bodyFunc :: Request -> Request;
					   bodyFunc = urlEncodedBody
					   	[("link_id", B8.pack $ link ^. name),
					   		("api_type", "json"),
					   		("children", B8.pack $ intercalate "," (mc ^. Ty.children))]
   	in do
   		request <- fmap bodyFunc (parseUrl $ "http://www.reddit.com/api/morechildren.json")
   		req' <- fillRequest request
   		resp <- withManager $ httpLbs request
   		let v = responseBody resp
   		let j :: Either String Value
   		    j = eitherDecode v
   		let arr = V.toList <$> (v ^? key "json" . key "data" . key "things" . _Array)
		let arr' =  (map (\x -> fromJust $ x ^? key "data")) $ fromJust arr
		let cs = map (getCommentOrKids . Just) arr'
   		return cs

continue :: Link -> MoreChildren -> Reddit [Either MoreChildren Comment]
continue link mc = let bodyFunc :: Request -> Request;
					   bodyFunc = urlEncodedBody
					   	[("link_id", B8.pack $ link ^. name),
					   		("api_type", "json"),
					   		("children", B8.pack $ intercalate "," (mc ^. Ty.children))]
   	in do
   		request <- (parseUrl $ link ^. url ++ (drop 3 (mc ^. parentId)) ++ ".json")
   		req' <- fillRequest request
   		resp <- withManager $ httpLbs request
   		let v = responseBody resp
   		let j :: Either String Value
   		    j = eitherDecode v
		let tq = j ^? _Right . _Array . ix 1 . key "data" . key "children" . _Array . ix 0
		let tq' = fromMaybe (error "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA") tq
		let cs = freeChildren tq'
   		return cs

freeChildren :: Value -> [Either MoreChildren Comment]
freeChildren j = me : kids where
	v = j ^? key "data"
	kids' = do
		l <- j ^? key "data" . key "replies" . key "data" . key "children" . _Array
		Just $ V.toList l
	me = getCommentOrKids v
	kids'' = case kids' of
			Just k -> k
			Nothing -> []
	kids = concatMap freeChildren kids''

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

thing :: Thread -> ([Comment], [MoreChildren])
thing = splitUp . flatThread

flatThread f = concat $ flatten <$> f
splitUp coms = (rights coms, lefts coms)

partBy (Right comment) = True
partBy _ = False

sBy x y = (y ^. ups - y ^. downs) `compare` (x ^. ups - x ^. downs)

getAllKids :: Link -> [MoreChildren] -> Reddit [Either MoreChildren Comment]
getAllKids link mcs = do
	vs <- mapM (moreKids link) mcs
	return $ concat vs

wookie :: Link -> Thread -> Reddit ([Either MoreChildren Comment], [Either MoreChildren Comment])
wookie link f = do
	let (oldComments, oldMores) = thing f
	moreComments <- getAllKids link oldMores
	let (newMores, newComments) = (lefts moreComments, rights moreComments)
	let allNewComments = (oldComments) ++ (newComments)
	let (newRootComments, otherComments) = (cs, os) where
		(cs, os) = partition (\x -> take 2 (x ^. parentId) == "t3") allNewComments
	let (newRootMores, otherMores) = partition (\x -> take 2 (x ^. parentId) == "t3") newMores
	let (newRoots, newOthers) = (map Right newRootComments ++ map Left newRootMores,
		map Right otherComments ++ map Left otherMores)
	return (nub newRoots, nub newOthers)


garble link f = do
	(roots, others) <- wookie link f
	return $ unfoldForest (helper others) roots where
		helper os x = (x, filter (\y -> either (^. parentId) (^. parentId) y ==
			either (^. name) (^. name) x) os)

buildTree root others = unfoldTree (helper others) root where
	helper os x = (x, filter (\y -> either (^. parentId) (^. parentId) y ==
			either (^. name) (^. name) x) os)

expandNode link base mc = do
	newCommentItems <- moreKids link mc
	return $ buildTree base newCommentItems