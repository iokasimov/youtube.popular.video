module Types.Videoinfo (Videoinfo(..), API(..), Types.Videoinfo.videoinfo, endpoint) where

import Data.Proxy
import Data.Aeson
import qualified Data.Text as T
import Servant.Client
import Servant.API

import Types.Video

type Text = T.Text
------------------------------------------------------------------------------------------------

data Videoinfo = Videoinfo { nextPageToken :: Maybe Text, items :: [Video] } deriving Show

instance FromJSON Videoinfo where
	parseJSON (Object o) = Videoinfo <$> o .:? "nextPageToken" <*> o .: "items"

type API = "videos" :>
	QueryParam "key" Text :>
	QueryParam "id" Text :>
	QueryParam "part" Text :>
	QueryParam "pageToken" Text :>
	Get '[JSON] Videoinfo

videoinfo :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ClientM Videoinfo
videoinfo = client (Proxy :: Data.Proxy.Proxy API)

endpoint :: Text -> Text -> Text -> ClientM Videoinfo
endpoint ids pg_token apikey = videoinfo (Just apikey) (Just ids)
	(Just "snippet,contentDetails,statistics") (Just pg_token)