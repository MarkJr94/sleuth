{-# LANGUAGE OverloadedStrings #-}

module User(comments) where

import           Control.Applicative
import           Control.Monad.State
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy as L
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
import           Util

comments :: String -> SessionState (Either String [Value])
comments user = do
    req' <- fmap addUAString (parseUrl $ printf "http://reddit.com/user/%s/comments.json" user)
    cs <- get
    let req = req' { cookieJar = Just newJar } where
        newJar = createCookieJar cs
    jVal <- fmap (eitherDecode . responseBody) (withManager $ httpLbs req)
    let listing :: Either String Co.Listing;
        listing = jVal >>= (\x -> parseEither (x .:) (T.pack "data"))
    let valList = fmap (\ls -> map Co.data_ $ Co.children ls) listing
    --return $ case valList of
                --Left err -> Left err
                --Right vals -> Right $ map (resultToEither . fromJSON) vals
                --Right vals -> (\v -> parseEither parseJSON v) (head vals)
    --return $ fmap (\vs -> (head vs) : []) valList
    return valList
