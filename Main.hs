module Main where

import Pipes
import Data.List
import Data.Proxy
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.TLS
import Options.Applicative
import Servant.Client
import Servant.API

import qualified Types.Video as Video
import qualified Types.Popular as Popular
import qualified Types.Related as Related
import qualified Types.Videoinfo as Videoinfo
import qualified Parsers.Duration as Duration
import qualified Types.Categories as Categories

type Text = T.Text
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

textify :: Text -> Video -> Text
textify cat_name video = cat_name
	<> "|" <> ((Video.channelTitle . Video.snippet) video)
	<> "|" <> ((Video.title . Video.snippet) video)
	<> "|" <> (maybe "" id $ Video.statistics video >>= Video.views)
	<> "|" <> (maybe "" id $ Video.statistics video >>= Video.likes)
	<> "|" <> (maybe "" id $ Video.statistics video >>= Video.dislikes)
	<> "|" <> (maybe "" id $ Video.statistics video >>= Video.comments)
	<> "|" <> (maybe "" id $ Video.content video >>= Duration.extract . Video.duration)

load_popular :: ClientEnv -> APIkey -> Text -> Category -> IO ()
load_popular env (APIkey key) pg_token cat = do
	result <- runClientM (Popular.endpoint 1 pg_token $ T.pack key) env
	either (\err -> return ()) go result where
		
		go :: Popular -> IO ()
		go videos = do
			sequence_ $ save_id <$> (Popular.items videos)
			sequence_ $ save_info <$> (Popular.items videos)
			case Popular.nextPageToken videos of
				Just token -> load_popular env (APIkey key) token cat
				Nothing -> return ()

		save_id :: Video -> IO ()
		save_id video = T.appendFile "Temporary/popular.ids.csv" $ 
			(Video.id' video) <> "\n"

		save_info :: Video -> IO ()
		save_info video = T.appendFile "Temporary/result.csv" $ 
			(textify (Categories.title cat) video) <> "\n"

load_related :: Text -> ClientEnv -> APIkey -> Text -> IO ()
load_related video_id env (APIkey key) pg_token = do
	result <- runClientM (Related.endpoint video_id pg_token $ T.pack key) env
	either (\err -> print err) go result where

		go :: Related -> IO ()
		go related = do
			load_video_info $ Related.items related
			case Related.nextPageToken related of
				Just token -> load_related video_id env (APIkey key) token
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

process :: ClientEnv -> APIkey -> IO ()
process env apikey = do
	sequence_ $ load_popular env apikey "" <$> Categories.all
	runEffect $ ids >-> loading env apikey where

	ids :: Producer Text IO ()
	ids = lift (T.readFile "Temporary/popular.ids.csv") >>= \csv ->
		sequence_ $ yield <$> T.lines csv where

	loading :: ClientEnv -> APIkey -> Consumer Text IO ()
	loading env apikey = forever $ await >>= \vid ->
		lift $ load_related vid env apikey ""


------------------------------------------------------------------------------------------------

data APIkey = APIkey { apikey' :: String }

cmdargs :: Parser APIkey
cmdargs = APIkey <$> argument str (metavar "apikey")

main = do
	apikey <- execParser (info cmdargs mempty)
	manager <- newManager tlsManagerSettings
	let env = ClientEnv manager settings
	process env apikey




