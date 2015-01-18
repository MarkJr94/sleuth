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
import qualified Data.Foldable              as F
import qualified Data.HashMap.Strict        as HS
import qualified Data.List                  as DL
import qualified Data.Text                  as T
import qualified Data.Tree                  as TR
import           Network.HTTP.Conduit
import           System.IO
import           Text.Printf                (printf)

import qualified Types
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


aboutIO :: String -> IO Account
aboutIO user = do
    req <- parseUrl $ printf "http://www.reddit.com/user/%s/about.json" user
    resp <- withManager $ httpLbs req
    let body = responseBody resp
    let jVal = eitherDecode body
    return $ eitherToException $ jVal >>= (\x -> parseEither (x .:) "data")

main :: IO ()
main = do
    about <- aboutIO "urdnot_rekt"
    print "Got `about` `urdnot_rekt` successfully"

    handle <- openFile "secrets.txt" ReadMode
    [user, pass] <- sequence [hGetLine handle, hGetLine handle]
    print $ user ++ " " ++ pass

    state <- login user pass
    evalStateT thing state

thing :: Reddit ()
thing = do
    me <- aboutMe
    liftIO $ print "Got `aboutMe` successfully. (Means auth is good)"
    comments <- U.comments "Suppiluliuma_I" New DefaultAge $$ CL.take 1
    submitted <- U.submitted "Suppiluliuma_I" New DefaultAge $$ CL.take 1
    liked <- U.liked "Suppiluliuma_I" $$ CL.take 1
    let sample = head $ head liked
    thread <- Th.thread sample
    fullThread <- Th.garble sample thread

    
    liftIO $ do
        putStrLn "\n"
        print $ head $ head comments
        putStrLn "\n"
        print $ head $ head submitted
        putStrLn "\n"
        print $ sample
        putStrLn "\n"
        putStrLn $ Th.drawThread thread
        putStrLn $ (show $ countForest thread) ++ " comments total."

        --putStrLn "\n"
        --print $ (\x -> x ^. ups - x ^. downs) <$> fst (Th.thing thread)

        putStrLn "\n"
        putStrLn $ Th.drawThread fullThread
        putStrLn $ (show $ countForest fullThread) ++ " comments total."

getRight x = case x of
    Left _ -> error "Left in getRight"
    Right x -> x



countForest :: TR.Forest (Either a b) -> Int
countForest f =  sum $ (F.foldr (\elem acc -> case elem of
    Left _ -> acc
    Right _ -> acc + 1) 0 ) <$> f