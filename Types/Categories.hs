module Types.Categories (Category(..), all) where

import Prelude hiding (all)
import qualified Data.Text as T

type Text = T.Text
data Category = Category { title :: Text , cid :: Int }

all :: [Category]
all = Category "Кино и анимация" 1
	: Category "Авто и транспорт" 2 
	: Category "Музыка" 10 
	: Category "Животные и питомцы" 15 
	: Category "Спорт" 17 
	: Category "Короткометражки" 18 
	: Category "События и путешествия" 19 
	: Category "Игры" 20 
	: Category "Видеоблоггеры" 21 
	: Category "Люди и блоги" 22 
	: Category "Комедии" 23
	: Category "Развлечения" 24 
	: Category "Новости и политика" 25 
	: Category "Стиль и лайфхаки" 26 
	: Category "Образование" 27 
	: Category "Наука и технологии" 28 
	: Category "Фильмы" 30 
	: Category "Аниме и анимация" 31 
	: Category "Боевики и приключения" 32 
	: Category "Классика" 33 
	: Category "Комедии" 34 
	: Category "Документальное" 35 
	: Category "Драмы" 36 
	: Category "Семья" 37 
	: Category "Зарубежное" 38 
	: Category "Ужасы" 39 
	: Category "Фантастика" 40 
	: Category "Триллеры" 41 
	: Category "Короткие" 42 
	: Category "Шоу" 43 
	: Category "Трейлеры" 44
	: []