{-# LANGUAGE OverloadedStrings #-}

module Auth( SessionState,
    login,
    aboutMe,
    pushCookie,
    logout,
    auth) where

import           Control.Applicative
import           Control.Monad.State
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.UTF8 as BU
import           Data.List            (find)
import           Data.Maybe           (fromJust)
import qualified Data.Text            as T
import           Network.HTTP.Conduit
import           Network.HTTP.Types
import           Common

import           RedditData.Account


type SessionState = StateT CookieJar IO

pushCookie :: CookieJar -> SessionState ()
pushCookie cs = state $ const ((), cs)

--getCookie :: SessionState (Maybe Cookie)
--getCookie = state $ \c -> (c, c)

--extractCookie :: SessionState (Maybe Cookie)
--extractCookie = state $ \c -> (c, Nothing)

login :: String  -> String -> IO CookieJar
login user pass = let bodyFunc :: Request -> Request;
                      bodyFunc = urlEncodedBody
                        [("user", BU.fromString user), ("passwd", BU.fromString pass), ("api_type", "json"), ("rem", "false") ]
    in do
        request <- fmap (addUAString . bodyFunc)
            (parseUrl $ "http://www.reddit.com/api/login/" ++ user)
        fmap responseCookieJar
            (withManager $ httpLbs request)

auth :: String  -> String -> SessionState ()
auth user pass = let bodyFunc :: Request -> Request;
                      bodyFunc = urlEncodedBody
                        [("user", BU.fromString user), ("passwd", BU.fromString pass), ("api_type", "json"), ("rem", "false") ]
    in do
        request <- fmap (addUAString . bodyFunc)
            (parseUrl $ "http://www.reddit.com/api/login/" ++ user)
        cj <- fmap responseCookieJar
            (withManager $ httpLbs request)
        put cj

aboutMe :: SessionState (Either String Account)
aboutMe = do
    req' <- fmap addUAString (parseUrl "http://reddit.com/api/me.json")
    cs <- get
    let req = req' { cookieJar = Just cs }
    jVal <- fmap (eitherDecode . responseBody) (withManager $ httpLbs req)
    return $ jVal >>= (\x -> parseEither (x .:) (T.pack "data"))

logout :: Int
logout = 2

