module Cache (check) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import Control.Error.Util
import Database.Redis

type Text = T.Text
type Bytes = B.ByteString

check :: Connection -> Text -> IO (Maybe Text)
check connection key = existing (T.encodeUtf8 key) >>= return . injecting . hush where

	-- check that this key exist
	existing :: Bytes -> IO (Either Reply Bool)
	existing video_id = runRedis connection $ 
		exists video_id >>= \r -> case r of
			Right False -> set video_id B.empty >> return r
			otherwise -> return r

	-- if key not exist then return just key
	injecting :: Maybe Bool -> Maybe Text
	injecting (Just False) = Just key
	injecting _ = Nothing