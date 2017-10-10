module Types.Related (Related(..), API(..), Videolink(..), Id(..), Types.Related.related, endpoint, download) where

import Data.List
import Data.Proxy
import Data.Aeson
import Data.Monoid
import Data.Witherable
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Machine
import Control.Monad.Trans
import Database.Redis
import Servant.Client
import Servant.API

import qualified Cache as Cache
import qualified Configuration as Configuration
import qualified Types.Video as Video
import qualified Types.Popular as Popular
import qualified Types.Videoinfo as Videoinfo
import qualified Parsers.Duration as Duration
import qualified Types.Category as Category

type Text = T.Text
type Video = Video.Video
type Videoinfo = Videoinfo.Videoinfo
type Configuration = Configuration.Configuration

------------------------------------------------------------------------------------------------

data Related = Related { token :: Maybe Text, items :: [Videolink] } deriving Show

instance FromJSON Related where
	parseJSON (Object o) = Related <$> o .:? "nextPageToken" <*> o .: "items"

------------------------------------------------------------------------------------------------

data Videolink = Videolink { videoId :: Id } deriving Show

instance FromJSON Videolink where
	parseJSON (Object o) = Videolink <$> o .: "id"

------------------------------------------------------------------------------------------------

data Id = Id { id' :: Text } deriving Show

instance FromJSON Id where
	parseJSON (Object o) = Id <$> o .: "videoId"

------------------------------------------------------------------------------------------------

type API = "search" :>
	QueryParam "key" Text :>
	QueryParam "part" Text :>
	QueryParam "type" Text :>
	QueryParam "relatedToVideoId" Text :>
	QueryParam "maxResults" Text :>
	QueryParam "pageToken" Text :>
	Get '[JSON] Related

related :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ClientM Related
related = client (Proxy :: Data.Proxy.Proxy API)

endpoint :: Text -> Text -> Text -> ClientM Related
endpoint vid' token' apikey = related
	(Just apikey) (Just "snippet") (Just "video")
	(Just vid') (Just "50") (Just token')

------------------------------------------------------------------------------------------------

loading :: Configuration -> Text -> Text -> IO ()
loading cfg token' vid' = do
	result <- runClientM (endpoint vid' token' $ Configuration.apikey cfg) $ Configuration.env cfg
	either print continue result where

	continue :: Related -> IO ()
	continue related = do
		filtered_keys <- wither (Cache.check $ Configuration.connection cfg) $
			(id' . videoId) <$> items related
		Videoinfo.download cfg token' $ map (id' . videoId) $ 
			Data.List.filter (\v -> elem (id' $ videoId $ v) filtered_keys)
			(items related)

		maybe (return ()) (\t -> loading cfg t vid') $ token related

download :: Configuration -> ProcessT IO Text ()
download cfg = repeatedly $ await >>= lift . loading cfg ""
