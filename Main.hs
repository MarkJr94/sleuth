module Main where

import           Auth
import           Common
import           Control.Applicative
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
import           Network.HTTP.Conduit
import           RedditData.Account
import           RedditData.Comment         as C
import qualified RedditData.Common          as Co
import           System.IO
import           Text.Printf                (printf)
import qualified User                       as U

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
    jVal <- fmap (eitherDecode . responseBody) $ withManager $ httpLbs req
    return $ jVal >>= (\x -> parseEither (x .:) (T.pack "data"))

main :: IO ()
main = do
    about <- aboutIO "urdnot_rekt"
    case about of
        Right _ -> print "Got `about` `urdnot_rekt` successfully"
        _ -> return ()

    handle <- openFile "secrets.txt" ReadMode

    [user, pass] <- sequence [hGetLine handle, hGetLine handle]
    --print (user, pass)
    cookies <- login user pass
    --print cookies

    me <- evalStateT aboutMe cookies
    case me of
        Right _ -> print "Got `aboutMe` successfully. (Means auth is good)"
        _ -> return ()

    let th = (U.comments "Suppiluliuma_I" Co.New Co.DefaultAge) $$ sink
    evalStateT th cookies

sink :: (Sink (Either String [Comment]) SessionState ())
sink = CL.mapM_ (\cs ->  do
    liftIO $ do
        putStrLn "\n\n ENTER `y` to get next page\n"
        l <- getLine
        if l == "y"
            then print cs
            else return ())
