module Types.Video (Video(..), Snippet(..), Content(..), Statistics(..), textify, save) where

import Data.Aeson
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Parsers.Duration as Duration
import qualified Types.Category as Category

type Text = T.Text
type Category = Category.Category

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

------------------------------------------------------------------------------------------------

textify :: Text -> Video -> Text
textify cat_name video = cat_name
	<> "|" <> ((channelTitle . snippet) video)
	<> "|" <> ((title . snippet) video)
	<> "|" <> (maybe "" id $ statistics video >>= views)
	<> "|" <> (maybe "" id $ statistics video >>= likes)
	<> "|" <> (maybe "" id $ statistics video >>= dislikes)
	<> "|" <> (maybe "" id $ statistics video >>= comments)
	<> "|" <> (maybe "" id $ content video >>= Duration.extract . duration)

save :: Category -> Video -> IO ()
save cat video = T.appendFile "Temporary/result.csv" $ 
	(textify (Category.title cat) video) <> "\n"
