module Main where

import Data.Proxy
import Data.Machine
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import Control.Monad
import Control.Monad.Trans
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.TLS
import Database.Redis
import Servant.Client
import Servant.API

import qualified Configuration as Configuration
import qualified Types.Popular as Popular
import qualified Types.Related as Related
import qualified Types.Videoinfo as Videoinfo
import qualified Types.Category as Category

type Text = T.Text
type Popular = Popular.Popular
type Related = Related.Related
type Videoinfo = Videoinfo.Videoinfo
type Configuration = Configuration.Configuration

------------------------------------------------------------------------------------------------

type API = Popular.API :<|> Related.API :<|> Videoinfo.API

api = Proxy :: Data.Proxy.Proxy API
settings = BaseUrl Https "www.googleapis.com" 443 "/youtube/v3"

videos :<|> related :<|> videoinfo = client api

------------------------------------------------------------------------------------------------

main = do
	manager <- newManager tlsManagerSettings
	let env = ClientEnv manager settings
	cfg <- Configuration.initialize env
	print "Downloading started ..."
	sequence_ $ Popular.download cfg "" <$> Category.all
	runT_ $ Related.download cfg <~ Popular.datasource