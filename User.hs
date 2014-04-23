{-# LANGUAGE OverloadedStrings #-}

module User(
      comments
    , submitted
    , liked
    , disliked
    , saved_
    , hidden_) where

import           Control.Applicative
import           Control.Lens
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
import           Types
import qualified Types
import           Common
 
comments :: String -> Popularity -> Age -> Source Reddit (Either String [Comment])
comments user pop age = 
    let first = do
            req'' <- lift $ join (fmap fillRequest 
                                    (parseUrl $
                                        printf "http://reddit.com/user/%s/comments.json" user))
            let baseQ = [("sort", Just $ formatPop pop), ("t", Just $ formatAge age)]
            let whoa' = whoa req'' baseQ
            yield (whoa', "")
        second = awaitForever (\(whoa', after) -> do
                    (cms, after') <- liftIO $ whoa' after
                    case after' of
                        Just after'' -> do
                            yield cms
                            leftover (whoa', after'')
                            second
                        Nothing -> return ())
    in
        first $= second

submitted :: String -> Popularity -> Age -> Source Reddit (Either String [Link])
submitted user pop age = 
    let first = do
            req'' <- lift $ join (fmap fillRequest 
                                        (parseUrl $ 
                                            printf "http://reddit.com/user/%s/submitted.json" user))
            let baseQ = [("sort", Just $ formatPop pop), ("t", Just $ formatAge age)]
            let whoa' = whoa req'' baseQ
            yield (whoa', "")
        second = awaitForever (\(whoa', after) -> do
                    (cms, after') <- liftIO $ whoa' after
                    case after' of
                        Just after'' -> do
                            yield cms
                            leftover (whoa', after'')
                            second
                        Nothing -> return ())
    in
        first $= second

liked :: String -> Source Reddit (Either String [Link])
liked user = commonHelperLink url where
    url = printf "http://reddit.com/user/%s/liked.json" user

disliked :: String -> Source Reddit (Either String [Link])
disliked user = commonHelperLink url where
    url = printf "http://reddit.com/user/%s/disliked.json" user

saved_ :: String -> Source Reddit (Either String [Link])
saved_ user = commonHelperLink url where
    url = printf "http://reddit.com/user/%s/saved.json" user

hidden_ :: String -> Source Reddit (Either String [Link])
hidden_ user = commonHelperLink url where
    url = printf "http://reddit.com/user/%s/hidden.json" user

commonHelperLink :: String -> Source Reddit (Either String [Link])
commonHelperLink url = 
    let first = do
            req'' <- lift $ join (fmap fillRequest (parseUrl url))
            let whoa' = whoa req'' []
            yield (whoa', "")
        second = awaitForever (\(whoa', after) -> do
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
whoa req' baseQuery after' = do
    let req = req' { queryString = newQ } where
            newQ = renderQuery False $  ("after", Just $ BU.fromString after')
                        : baseQuery

    jsonRes <- fmap (eitherDecode . responseBody) (withManager $ httpLbs req)
    let listing' :: Either String Listing;
        listing' = jsonRes >>= (\x -> parseEither (x .:) (T.pack "data"))

    let vs = fmap (\ls -> map (^. data_) $ ls ^. Types.children) listing'
    let rets = case vs of
                    Left err -> Left err
                    Right vs' -> mapM (resultToEither . fromJSON) vs'
    return (rets, fixUp listing') where
        fixUp (Right l) = case l ^. after of
                            Just a -> l ^. after
                            Nothing -> Nothing
        fixUp (Left _) = Nothing


addAuth :: Request -> Reddit Request
addAuth req = do
    cs <- get
    return req {cookieJar = Just $ cs ^. jar}

fillRequest :: Request -> Reddit Request
fillRequest = addAuth . addUAString