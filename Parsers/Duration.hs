module Parsers.Duration (extract) where

import Data.Char
import Control.Applicative
import Data.Attoparsec.Text
import Data.Monoid
import qualified Data.Text as T
import Prelude hiding (takeWhile)

type Text = T.Text
type Duration = Text

extract :: T.Text -> Maybe Duration
extract input = case parse pt input of
	Fail "" _ _ -> Nothing
	Fail rest _ _ -> extract $ T.tail rest
	Partial _ -> Nothing
	Done _ result -> Just result

pt :: Parser Duration
pt = "PT" *> (with_days <$> days <*> hours <*> minutes <*> seconds)
	<|> (with_hours <$> hours <*> minutes <*> seconds)
	<|> (with_minutes <$> minutes <*> seconds)
	<|> (with_seconds <$> seconds) where

		with_days _ hs ms ss = hs <> ":" <> ms <> ":" <> ss
		with_hours hs ms ss = hs <> ":" <> ms <> ":" <> ss
		with_minutes ms ss = "00:" <> ms <> ":" <> ss
		with_seconds ss = "00:00:" <> ss

days :: Parser Text
days = takeWhile isDigit <* "D"

hours :: Parser Text
hours = takeWhile isDigit <* "H"

minutes :: Parser Text
minutes = takeWhile isDigit <* "M"

seconds :: Parser Text
seconds = takeWhile isDigit <* "S"