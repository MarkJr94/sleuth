import           Auth                       
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy.UTF8  as LU
import qualified Data.ByteString.UTF8       as BU
import           Data.Conduit
import qualified Data.Conduit.Binary        as CB
import qualified Data.Conduit.List          as CL
import qualified Data.HashMap.Strict        as HS
import qualified Data.List                  as DL
import qualified Data.Text                  as T
import           Network.HTTP.Conduit
import           RedditData.Account                 
import           RedditData.Comment as C         
import           System.IO
import           Text.Printf                (printf)
import Util
import Control.Applicative
import Control.Monad.State
import qualified User as U

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
    print about

    [user, pass] <- sequence [getLine, getLine]
    --print (user, pass)
    cookies <- login user pass
    --print cookies

    me <- evalStateT aboutMe cookies
    --print me

    commentVals' <- evalStateT (U.comments "Suppiluliuma_I") cookies
    let commentVals = case commentVals' of
                    Left err -> error err
                    Right vals -> vals
    sequence $ map print commentVals
    let comments :: [Either String Comment];
        comments = map (\x -> parseEither parseJSON x) commentVals
    _ <- sequence $ map print comments
    return ()