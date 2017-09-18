module Types.Videos (Videos(..)) where

import Data.Aeson
import qualified Data.Text as T

type Text = T.Text

------------------------------------------------------------------------------------------------

data Videos = Videos { nextPageToken :: Text, items :: [Video] } deriving Show

instance FromJSON Videos where
	parseJSON (Object o) = Videos <$> o .: "nextPageToken" <*> o .: "items"

------------------------------------------------------------------------------------------------

data Video = Video { snippet :: Snippet, content :: Content, statistics :: Statistics } deriving Show

instance FromJSON Video where
	parseJSON (Object o) = Video
		<$> o .: "snippet"
		<*> o .: "contentDetails"
		<*> o .: "statistics"

------------------------------------------------------------------------------------------------

data Snippet = Snippet { title :: Text, channelTitle :: Text, categoryId :: Text } deriving Show

instance FromJSON Snippet where
	parseJSON (Object o) = Snippet
		<$> o .: "title" 
		<*> o .: "channelTitle"
		<*> o .: "categoryId"

------------------------------------------------------------------------------------------------

data Content = Content { duration :: Text } deriving Show

instance FromJSON Content where
	parseJSON (Object o) = Content <$> o .: "duration"

------------------------------------------------------------------------------------------------

data Statistics = Statistics { views :: Text, likes :: Text, dislikes :: Text, comments :: Text } deriving Show

instance FromJSON Statistics where
	parseJSON (Object o) = Statistics
		<$> o .: "viewCount"
		<*> o .: "likeCount"
		<*> o .: "dislikeCount"
		<*> o .: "commentCount"
