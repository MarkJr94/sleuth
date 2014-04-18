{-# LANGUAGE OverloadedStrings #-}

module User(
      comments
    , submitted
    , liked
    , disliked
    , saved
    , hidden) where

import           Control.Applicative
import           Control.Monad.State
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Conduit.List as DL
import Data.Conduit
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.UTF8 as LU
import qualified Data.ByteString.UTF8 as BU
import           Data.List            (find)
import           Data.Maybe           (fromJust)
import qualified Data.Text            as T
import Text.Printf (printf)
import           Network.HTTP.Conduit
import           Network.HTTP.Types

import           Auth
import qualified RedditData.Common as Co
import           RedditData.Account   
import           RedditData.Comment
import           RedditData.Link as RL
import           Common
 
comments :: String -> Co.Popularity -> Co.Age -> Source (SessionState) (Either String [Comment])
comments user pop age = 
    let first = do
            req'' <- lift $ do
                req' <- fmap addUAString 
                    (parseUrl $ printf "http://reddit.com/user/%s/comments.json" user)
                cs <- get
                let dummy = req' { cookieJar = Just cs }
                return dummy
            let baseQ = [("sort", Just $ Co.formatPop pop), ("t", Just $ Co.formatAge age)]
            let whoa' = whoa req'' baseQ
            yield (whoa', "")
        second = do
            awaitForever (\(whoa', after) -> do
                (cms, after') <- liftIO $ whoa' after
                case after' of
                    Just after'' -> do
                        yield cms
                        leftover (whoa', after'')
                        second
                    Nothing -> return ())
    in
        first $= second

--gilded :: String -> Source (SessionState) (Either String [Comment])
--gilded user =
--    let first = do
--            req'' <- lift $ do
--                req' <- fmap addUAString 
--                    (parseUrl $ printf "http://reddit.com/user/%s/gilded.json" user)
--                cs <- get
--                let dummy = req' { cookieJar = Just cs }
--                return dummy
--            let whoa' = whoa req'' []
--            yield (whoa', "")
--        second = do
--            awaitForever (\(whoa', after) -> do
--                (cms, after') <- liftIO $ whoa' after
--                case after' of
--                    Just after'' -> do
--                        yield cms
--                        leftover (whoa', after'')
--                        second
--                    Nothing -> return ())
--    in
--        first $= second

submitted :: String -> Co.Popularity -> Co.Age -> Source (SessionState) (Either String [Link])
submitted user pop age = 
    let first = do
            req'' <- lift $ do
                req' <- fmap addUAString 
                    (parseUrl $ printf "http://reddit.com/user/%s/submitted.json" user)
                cs <- get
                let dummy = req' { cookieJar = Just cs }
                return dummy
            let baseQ = [("sort", Just $ Co.formatPop pop), ("t", Just $ Co.formatAge age)]
            let whoa' = whoa req'' baseQ
            yield (whoa', "")
        second = do
            awaitForever (\(whoa', after) -> do
                (cms, after') <- liftIO $ whoa' after
                case after' of
                    Just after'' -> do
                        yield cms
                        leftover (whoa', after'')
                        second
                    Nothing -> return ())
    in
        first $= second

liked :: String -> Source (SessionState) (Either String [Link])
liked user = commonHelperLink url where
    url = printf "http://reddit.com/user/%s/liked.json" user

disliked :: String -> Source (SessionState) (Either String [Link])
disliked user = commonHelperLink url where
    url = printf "http://reddit.com/user/%s/disliked.json" user

saved :: String -> Source (SessionState) (Either String [Link])
saved user = commonHelperLink url where
    url = printf "http://reddit.com/user/%s/saved.json" user

hidden :: String -> Source (SessionState) (Either String [Link])
hidden user = commonHelperLink url where
    url = printf "http://reddit.com/user/%s/hidden.json" user

commonHelperLink :: String -> Source (SessionState) (Either String [Link])
commonHelperLink url = 
    let first = do
            req'' <- lift $ do
                req' <- fmap addUAString 
                    (parseUrl url)
                cs <- get
                let dummy = req' { cookieJar = Just cs }
                return dummy
            let whoa' = whoa req'' []
            yield (whoa', "")
        second = do
            awaitForever (\(whoa', after) -> do
                (cms, after') <- liftIO $ whoa' after
                case after' of
                    Just after'' -> do
                        yield cms
                        leftover (whoa', after'')
                        second
                    Nothing -> return ())
    in
        first $= second

whoa :: (FromJSON a) => Request -> Query -> String
     -> IO (Either String [a], Maybe String)
whoa req' baseQuery after = do
    let req = req' { queryString = newQ } where
            newQ = renderQuery False $  ("after", Just $ BU.fromString after)
                        : baseQuery

    jsonRes <- fmap (eitherDecode . responseBody) (withManager $ httpLbs req)
    let listing' :: Either String Co.Listing;
        listing' = jsonRes >>= (\x -> parseEither (x .:) (T.pack "data"))

    let vs = fmap (\ls -> map Co.data_ $ Co.children ls) listing'
    let rets = case vs of
                    Left err -> Left err
                    Right vs' -> sequence (map (resultToEither . fromJSON) vs')
    return (rets, fixUp listing' >>= Co.after) where
        fixUp (Right l) = case (Co.after l) of
                            Just a -> Just l
                            Nothing -> Nothing
        fixUp (Left _) = Nothing

