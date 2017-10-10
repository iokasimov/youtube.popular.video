module Configuration (Configuration(..), initialize) where

import qualified Data.Text as T
import Options.Applicative
import Database.Redis hiding (info)
import Servant.Client
import Servant.API

------------------------------------------------------------------------------------------------

data APIkey = APIkey String

cmdargs :: Parser APIkey
cmdargs = APIkey <$> argument str (metavar "apikey")

------------------------------------------------------------------------------------------------

data Configuration = Configuration { connection :: Connection, env :: ClientEnv, apikey :: T.Text }

initialize :: ClientEnv -> IO Configuration
initialize env = do
	APIkey apikey' <- execParser (info cmdargs mempty)
	connection <- checkedConnect defaultConnectInfo
	return $ Configuration connection env (T.pack apikey')