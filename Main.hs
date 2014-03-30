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
import           RedditData                 (Account)
import           System.IO
import           Text.Printf                (printf)
import Util
import Control.Applicative
import Control.Monad.State

add3 x y z = x + y + z
x = Just 1
y = Just 2
z = Just 3
t = add3 <$>  x <*> y <*> z
vs = [1..10]
summed = getZipList $ add3 <$> ZipList vs <*> ZipList vs <*> ZipList vs


aboutIO :: String -> IO (Either String Account)
aboutIO user = do
    req <- parseUrl $ printf "http://www.reddit.com/user/%s/about.json" user
    jVal <- fmap (eitherDecode . responseBody) $ withManager $ httpLbs req
    return $ jVal >>= (\x -> parseEither (x .:) (T.pack "data"))

main = do
    about <- aboutIO "Suppiluliuma_I"
    print about

    cookies <- login "Suppiluliuma_I" "z2Ltx1vYR1p5"
    print cookies

    me <- evalStateT aboutMe cookies
    print me