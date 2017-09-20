module Types.Video (Video(..), Snippet(..), Content(..), Statistics(..),) where

import Data.Aeson
import qualified Data.Text as T

type Text = T.Text

------------------------------------------------------------------------------------------------

data Video = Video { id' :: Text, snippet :: Snippet, content :: Maybe Content, statistics :: Maybe Statistics } deriving Show

instance FromJSON Video where
	parseJSON (Object o) = Video
		<$> o .: "id"
		<*> o .: "snippet"
		<*> o .:? "contentDetails"
		<*> o .:? "statistics"

------------------------------------------------------------------------------------------------

data Snippet = Snippet { title :: Text, channelTitle :: Text, categoryId :: Maybe Text } deriving Show

instance FromJSON Snippet where
	parseJSON (Object o) = Snippet
		<$> o .: "title" 
		<*> o .: "channelTitle"
		<*> o .:? "categoryId"

------------------------------------------------------------------------------------------------

data Content = Content { duration :: Text } deriving Show

instance FromJSON Content where
	parseJSON (Object o) = Content <$> o .: "duration"

------------------------------------------------------------------------------------------------

data Statistics = Statistics { views :: Maybe Text, likes :: Maybe Text, dislikes :: Maybe Text, comments :: Maybe Text } deriving Show

instance FromJSON Statistics where
	parseJSON (Object o) = Statistics
		<$> o .:? "viewCount"
		<*> o .:? "likeCount"
		<*> o .:? "dislikeCount"
		<*> o .:? "commentCount"
