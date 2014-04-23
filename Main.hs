{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Auth
import           Common
import           Control.Applicative
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy.UTF8  as LU
import qualified Data.ByteString.UTF8       as BU
import           Data.Conduit
import qualified Data.Conduit.List          as CL
import qualified Data.HashMap.Strict        as HS
import qualified Data.List                  as DL
import qualified Data.Text                  as T
import qualified Data.Tree                  as TR
import           Network.HTTP.Conduit
import           System.IO
import           Text.Printf                (printf)

import           Types
import qualified User                       as U
import qualified Thread as Th

add3 x y z = x + y + z
x = Just 1
y = Just 2
z = Just 3
t = add3 <$>  x <*> y <*> z
vs = [1..10]
summed :: [Int]
summed = getZipList $ add3 <$> ZipList vs <*> ZipList vs <*> ZipList vs


aboutIO :: String -> IO (Either String Account)
aboutIO user = do
    req <- parseUrl $ printf "http://www.reddit.com/user/%s/about.json" user
    resp <- withManager $ httpLbs req
    let body = responseBody resp
    let jVal = eitherDecode body
    return $ jVal >>= (\x -> parseEither (x .:) "data")

main :: IO ()
main = do
    about <- aboutIO "urdnot_rekt"
    case about of
        Right _ -> print "Got `about` `urdnot_rekt` successfully"
        _ -> return ()

    handle <- openFile "secrets.txt" ReadMode
    [user, pass] <- sequence [hGetLine handle, hGetLine handle]
    print $ user ++ " " ++ pass

    state <- login user pass
    evalStateT thing state

thing :: Reddit ()
thing = do
    me <- aboutMe
    comments <- U.comments "Suppiluliuma_I" New DefaultAge $$ CL.take 1
    submitted <- U.submitted "Suppiluliuma_I" New DefaultAge $$ CL.take 1
    liked <- U.liked "Suppiluliuma_I" $$ CL.take 1
    let sample = (head . getRight) $ head liked
    thread <- Th.thread sample

    
    liftIO $ do
        case me of
            Right _ -> print "Got `aboutMe` successfully. (Means auth is good)"
            _ -> return () 
        putStrLn "\n"
        print $ head <$> head comments
        putStrLn "\n"
        print $ head <$> head submitted
        putStrLn "\n"
        print $ head <$> head liked
        putStrLn "\n"
        putStrLn $ TR.drawForest $ map ((\x -> case x of
            Left _ -> "[MORE HIDDEN]"
            Right x -> x ^. author) <$>) thread
    return ()

getRight x = case x of
    Left _ -> error "Left in getRight"
    Right x -> x