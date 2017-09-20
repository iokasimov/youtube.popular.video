module Types.Related (Related(..), API(..), Videolink(..), Id(..), Types.Related.related, endpoint) where

import Data.Proxy
import Data.Aeson
import qualified Data.Text as T
import Servant.Client
import Servant.API

type Text = T.Text

------------------------------------------------------------------------------------------------

data Related = Related { nextPageToken :: Maybe Text, items :: [Videolink] } deriving Show

instance FromJSON Related where
	parseJSON (Object o) = Related <$> o .:? "nextPageToken" <*> o .: "items"

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
endpoint video_id pg_token apikey = related
	(Just apikey) (Just "snippet") (Just "video")
	(Just video_id) (Just "50") (Just pg_token)

------------------------------------------------------------------------------------------------

data Videolink = Videolink { videoId :: Id } deriving Show

instance FromJSON Videolink where
	parseJSON (Object o) = Videolink <$> o .: "id"

------------------------------------------------------------------------------------------------

data Id = Id { id' :: Text } deriving Show

instance FromJSON Id where
	parseJSON (Object o) = Id <$> o .: "videoId"