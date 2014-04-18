{-# LANGUAGE OverloadedStrings #-}

module User(comments
    , ) where

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
import           Common
 
comments :: String -> Co.Popularity -> Co.Age -> Source (SessionState) (Either String [Comment])
comments user pop age = 
    let first = do
            req'' <- lift $ do
                req' <- fmap addUAString 
                    (parseUrl $ printf "http://reddit.com/user/%s/comments.json" user)
                cs <- get
                let dummy = req' { cookieJar = Just newJar } where
                    newJar = createCookieJar cs
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
        --error "sada"



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

isRight (Left _) = False
isRight (Right _) = True

fromEither :: Either a b -> b
fromEither (Left _) = error "fromEither with Left"
fromEither (Right x) = x

