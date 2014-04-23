{-# LANGUAGE OverloadedStrings #-}

module Auth( 
    login,
    aboutMe,
    logout) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.State
import           Data.Aeson
import           Data.Aeson.Lens    
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.UTF8 as BU
import           Data.List            (find)
import           Data.Maybe           (fromJust)
import qualified Data.Text            as T
import           Network.HTTP.Conduit
import           Network.HTTP.Types

import           Common
import           Types

login :: String  -> String -> IO RedditState
login user pass = let bodyFunc :: Request -> Request;
                      bodyFunc = urlEncodedBody
                        [("user", BU.fromString user), ("passwd", BU.fromString pass), ("api_type", "json"), ("rem", "false") ]
    in do
        request <- fmap (addUAString . bodyFunc)
            (parseUrl $ "http://www.reddit.com/api/login/" ++ user)
        resp <- withManager $ httpLbs request
        let cookies = responseCookieJar resp
        let mhash = responseBody resp ^? key "json" . key "data" . key "modhash" . _String
        let mhash' = case mhash of
                        Just s -> T.unpack s
                        Nothing -> error "No modhash given on login!"
        return RedditState { _redditstateJar = cookies, _redditstateModhash = mhash'}

aboutMe :: Reddit (Either String Account)
aboutMe = do
    req' <- fmap addUAString (parseUrl "http://reddit.com/api/me.json")
    rs <- get
    let req = req' { cookieJar = Just $ rs ^. jar }
    jVal <- fmap (eitherDecode . responseBody) (withManager $ httpLbs req)
    return $ jVal >>= (\x -> parseEither (x .:) (T.pack "data"))

logout :: Int
logout = 2

