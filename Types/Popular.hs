module Types.Popular (Popular(..), API(..), Types.Popular.videos, endpoint, download, datasource) where

import Data.List
import Data.Proxy
import Data.Aeson
import Data.Monoid
import Data.Witherable
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Machine
import Control.Monad.Trans
import Servant.Client
import Servant.API

import qualified Cache as Cache
import qualified Configuration as Configuration
import qualified Types.Video as Video
import qualified Types.Category as Category

type Text = T.Text
type Video = Video.Video
type Category = Category.Category
type Configuration = Configuration.Configuration

------------------------------------------------------------------------------------------------

data Popular = Popular { token :: Maybe Text, items :: [Video] } deriving Show

instance FromJSON Popular where
	parseJSON (Object o) = Popular <$> o .:? "nextPageToken" <*> o .: "items"

------------------------------------------------------------------------------------------------

type API = "videos" :>
	QueryParam "key" Text :>
	QueryParam "chart" Text :>
	QueryParam "videoCategoryId" Int :>
	QueryParam "regionCode" Text :>
	QueryParam "part" Text :>
	QueryParam "maxResults" Text :>
	QueryParam "pageToken" Text :>
	Get '[JSON] Popular

videos :: Maybe Text -> Maybe Text -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ClientM Popular
videos = client (Proxy :: Data.Proxy.Proxy API)

endpoint :: Int -> Text -> Text -> ClientM Popular
endpoint cat_id token' apikey = videos
	(Just apikey) (Just "mostPopular") (Just cat_id) (Just "RU")
	(Just "snippet,contentDetails,statistics") (Just "50") (Just token')

------------------------------------------------------------------------------------------------

download :: Configuration -> Text -> Category -> IO ()
download cfg token' cat = do
	result <- runClientM (endpoint (Category.cid cat) token' $ Configuration.apikey cfg) $ Configuration.env cfg
	either print continue result where

		continue :: Popular -> IO ()
		continue popular = do
			-- remove those keys that already exists in cache
			filtered_keys <- wither (Cache.check $ Configuration.connection cfg) $ Video.id' <$> items popular
			let filtered_videos = Data.List.filter
				(\v -> elem (Video.id' v) filtered_keys)
				(items popular)
			sequence_ $ save_id <$> filtered_videos
			sequence_ $ Video.save cat <$> filtered_videos
			maybe (return ()) (\t -> download cfg t cat) $ token popular

		save_id :: Video -> IO ()
		save_id video = T.appendFile "Temporary/popular.ids.csv" $ 
			(Video.id' video) <> "\n"

datasource :: SourceT IO Text
datasource = construct $ lift (T.readFile "Temporary/popular.ids.csv") >>= 
	\csv -> sequence_ $ yield <$> T.lines csv
