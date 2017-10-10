module Types.Videoinfo (Videoinfo(..), API(..), videoinfo, endpoint, download) where

import Data.List
import Data.Proxy
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Servant.Client
import Servant.API

import qualified Configuration as Configuration
import qualified Types.Video as Video
import qualified Types.Category as Category

type Text = T.Text
type Video = Video.Video
type Configuration = Configuration.Configuration

------------------------------------------------------------------------------------------------

data Videoinfo = Videoinfo { nextPageToken :: Maybe Text, items :: [Video] } deriving Show

instance FromJSON Videoinfo where
	parseJSON (Object o) = Videoinfo <$> o .:? "nextPageToken" <*> o .: "items"

------------------------------------------------------------------------------------------------

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

------------------------------------------------------------------------------------------------

prepare :: Videoinfo -> Text
prepare = T.intercalate ""
	. map (\v -> flip mappend "\n" $ Video.textify (find_cat_name v) v)
	. items

-- МНЕ НЕ НРАВИТСЯ ЭТА ФУНКЦИЯ
find_cat_name :: Video -> Text
find_cat_name video = maybe "" Category.title $
	(Video.categoryId . Video.snippet) video >>= \cat ->
	find ((==(read (T.unpack cat) :: Int)) . Category.cid)
		Category.all

download :: Configuration -> Text -> [Text] -> IO ()
download cfg pg_token links = do
	result <- runClientM (endpoint (T.intercalate "," links) pg_token $ Configuration.apikey cfg) $ Configuration.env cfg
	T.appendFile "Temporary/result.csv" $ either (const "") prepare result
