module Types.Popular (Popular(..), API(..), Types.Popular.videos, endpoint) where

import Data.Proxy
import Data.Aeson
import qualified Data.Text as T
import Servant.Client
import Servant.API

import Types.Video

type Text = T.Text

------------------------------------------------------------------------------------------------

data Popular = Popular { nextPageToken :: Maybe Text, items :: [Video] } deriving Show

instance FromJSON Popular where
	parseJSON (Object o) = Popular <$> o .:? "nextPageToken" <*> o .: "items"

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
endpoint cat_id pg_token apikey = videos
	(Just apikey) (Just "mostPopular") (Just cat_id) (Just "RU")
	(Just "snippet,contentDetails,statistics") (Just "50") (Just pg_token)