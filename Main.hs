module Main where

import Data.Proxy
import qualified Data.Text as T
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.TLS
import Servant.API
import Servant.Client

import Types.Videos
import Types.Categories

type Text = T.Text
type Category = (Text,Int)

type API = "videos" :>
	QueryParam "key" Text :>
	QueryParam "chart" Text :>
	QueryParam "videoCategoryId" Int :>
	QueryParam "regionCode" Text :>
	QueryParam "part" Text :>
	QueryParam "pageToken" Text :>
	Get '[JSON] Videos

api = Proxy :: Data.Proxy.Proxy API
settings = BaseUrl Https "www.googleapis.com" 443 "/youtube/v3"

videos :: Maybe Text -> Maybe Text -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe Text -> ClientM Videos
videos = client api

-- https://www.googleapis.com/youtube/v3/videos
	-- ?chart=mostPopular
	-- &key=AIzaSyBf3H1gVh9TKKK2UJjYDkkOWPkyF6VWoG0
	-- &part=contentDetails,statistics
	-- &regionCode=RU
	-- &videoCategoryId=2
	-- &pageToken=CAUQAA

-- apikey = "AIzaSyBf3H1gVh9TKKK2UJjYDkkOWPkyF6VWoG0"
-- endpoint = "https://www.googleapis.com/youtube/v3/videos?chart=mostPopular&key=" 
-- 	<> apikey <> "&part=snippet,contentDetails,statistics&maxResults=4"

-- data Args = Args { region :: Int, apikey :: String } deriving Show

-- cmdargs :: Options.Applicative.Parser Args
-- cmdargs = Args <$> argument auto (metavar "region") <*> argument str (metavar "apikey")

endpoint :: Int -> Text -> ClientM Videos
endpoint cat_id pg_token = videos (Just "AIzaSyBf3H1gVh9TKKK2UJjYDkkOWPkyF6VWoG0") 
	(Just "mostPopular") (Just cat_id) (Just "RU") (Just "snippet,contentDetails,statistics")
	(Just "")

main = do
	manager <- newManager tlsManagerSettings -- HTTPS connection manager
	let env = ClientEnv manager settings -- environment for Servant requests
	res <- runClientM (endpoint 1 "") env
	print res
