module Main where

import Pipes
import Data.List
import Data.Proxy
import Data.Maybe
import Data.Monoid
import Data.Witherable
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import Control.Monad
import Control.Error.Util
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.TLS
import Options.Applicative
import Database.Redis
import Servant.Client
import Servant.API

import qualified Types.Video as Video
import qualified Types.Popular as Popular
import qualified Types.Related as Related
import qualified Types.Videoinfo as Videoinfo
import qualified Parsers.Duration as Duration
import qualified Types.Categories as Categories

type Text = T.Text
type Bytes = B.ByteString
type Popular = Popular.Popular
type Video = Video.Video
type Videoinfo = Videoinfo.Videoinfo
type Related = Related.Related
type Category = Categories.Category

type API = Popular.API :<|> Related.API :<|> Videoinfo.API

api = Proxy :: Data.Proxy.Proxy API
settings = BaseUrl Https "www.googleapis.com" 443 "/youtube/v3"

videos :<|> related :<|> videoinfo = client api

------------------------------------------------------------------------------------------------

cached :: Connection -> Text -> IO (Maybe Text)
cached connection key = existing (T.encodeUtf8 key) >>= return . injecting . hush where

	-- check that this key exist
	existing :: Bytes -> IO (Either Reply Bool)
	existing video_id = runRedis connection $ exists video_id >>= \r -> case r of
		Right False -> set video_id B.empty >> return r
		otherwise -> return r

	-- if key not exist then return whole branch
	injecting :: Maybe Bool -> Maybe Text
	injecting (Just False) = Just key
	injecting _ = Nothing


textify :: Text -> Video -> Text
textify cat_name video = cat_name
	<> "|" <> ((Video.channelTitle . Video.snippet) video)
	<> "|" <> ((Video.title . Video.snippet) video)
	<> "|" <> (maybe "" id $ Video.statistics video >>= Video.views)
	<> "|" <> (maybe "" id $ Video.statistics video >>= Video.likes)
	<> "|" <> (maybe "" id $ Video.statistics video >>= Video.dislikes)
	<> "|" <> (maybe "" id $ Video.statistics video >>= Video.comments)
	<> "|" <> (maybe "" id $ Video.content video >>= Duration.extract . Video.duration)

-- load a bunch of pupular videos
load_popular :: Connection -> ClientEnv -> APIkey -> Text -> Category -> IO ()
load_popular connection env (APIkey key) pg_token cat = do
	result <- runClientM (Popular.endpoint 1 pg_token $ T.pack key) env
	either (\err -> return ()) go result where
		
		go :: Popular -> IO ()
		go videos = do
			-- remove those keys that already exists in cache
			filtered_keys <- wither (cached connection) $ Video.id' <$> Popular.items videos
			let filtered_videos = Data.List.filter 
				(\v -> elem (Video.id' v) filtered_keys) 
				(Popular.items videos)
			sequence_ $ save_id <$> filtered_videos
			sequence_ $ save_info <$> filtered_videos
			case Popular.nextPageToken videos of
				Just token -> load_popular connection env (APIkey key) token cat
				Nothing -> return ()

		save_id :: Video -> IO ()
		save_id video = T.appendFile "Temporary/popular.ids.csv" $ 
			(Video.id' video) <> "\n"

		save_info :: Video -> IO ()
		save_info video = T.appendFile "Temporary/result.csv" $ 
			(textify (Categories.title cat) video) <> "\n"

-- load a bunch of related videos by id
load_related :: Connection -> Text -> ClientEnv -> APIkey -> Text -> IO ()
load_related connection video_id env (APIkey key) pg_token = do
	result <- runClientM (Related.endpoint video_id pg_token $ T.pack key) env
	either (\err -> print err) go result where

		go :: Related -> IO ()
		go related = do
			filtered_keys <- wither (cached connection) $
				(Related.id' . Related.videoId) <$> Related.items related
			load_video_info $ Data.List.filter 
				(\v -> elem (Related.id' $ Related.videoId $ v) filtered_keys)
				(Related.items related)
			case Related.nextPageToken related of
				Just token -> load_related connection video_id env (APIkey key) token
				Nothing -> return ()

		load_video_info :: [Related.Videolink] -> IO ()
		load_video_info vls = do
			let ids = T.intercalate "," $ (Related.id' . Related.videoId) <$> vls
			result <- runClientM (Videoinfo.endpoint ids pg_token $ T.pack key) env
			T.appendFile "Temporary/result.csv" $ either (const "") prepare result where

				prepare :: Videoinfo -> Text
				prepare = T.intercalate "" 
					. map (\v -> flip mappend "\n" $ textify (find_cat_name v) v)
					. Videoinfo.items

				find_cat_name :: Video -> Text
				find_cat_name video = maybe "" Categories.title $
					(Video.categoryId . Video.snippet) video >>= \cat ->
					find ((==(read (T.unpack cat) :: Int)) . Categories.cid)
						Categories.all

process :: Connection -> ClientEnv -> APIkey -> IO ()
process connection env apikey = do
	sequence_ $ load_popular connection env apikey "" <$> Categories.all
	runEffect $ ids >-> loading connection env apikey where

	ids :: Producer Text IO ()
	ids = lift (T.readFile "Temporary/popular.ids.csv") >>= \csv ->
		sequence_ $ yield <$> T.lines csv where

	loading :: Connection -> ClientEnv -> APIkey -> Consumer Text IO ()
	loading connection env apikey = forever $ await >>= \vid ->
		lift $ load_related connection vid env apikey ""

------------------------------------------------------------------------------------------------

data APIkey = APIkey { apikey' :: String }

cmdargs :: Parser APIkey
cmdargs = APIkey <$> argument str (metavar "apikey")

main = do
	apikey <- execParser (Options.Applicative.info cmdargs mempty)
	manager <- newManager tlsManagerSettings
	connection <- checkedConnect defaultConnectInfo
	let env = ClientEnv manager settings
	process connection env apikey





