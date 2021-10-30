module Notion.Client
  ( retrieveDatabase,
  )
where

------------------------------------------------------------------------------

import Data.Text (Text, unpack)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Notion.API as API
import Notion.Types (Database, DatabaseId)
import Notion.Utils
import Servant.API (toUrlPiece)
import Servant.Client (BaseUrl (..), ClientEnv, ClientError, Scheme (Https), mkClientEnv, runClientM)

------------------------------------------------------------------------------

-- | Helper function to run `API.retrieveDatabase`.
retrieveDatabase ::
  Maybe Text ->
  Maybe Version ->
  DatabaseId ->
  IO (Either ClientError Database)
retrieveDatabase authorizationHeader versionHeader databaseId = do
  env <- defaultEnv
  runClientM
    (API.retrieveDatabase authorizationHeader versionHeader databaseId)
    env

------------------------------------------------------------------------------

-- | The default `ClientEnv` for Notion API.
defaultEnv :: IO ClientEnv
defaultEnv = do
  manager <- newManager tlsManagerSettings
  pure $ mkClientEnv manager (mkBaseUrl Version_V1)

------------------------------------------------------------------------------

-- | Construct a `BaseUrl` for Notion API given a `NotionAPIVersion`.
mkBaseUrl :: Version -> BaseUrl
mkBaseUrl version =
  BaseUrl Https baseUrlHost baseUrlPort baseUrlPath
  where
    baseUrlHost = "api.notion.com/" <> (unpack $ toUrlPiece version)
    baseUrlPort = 443
    baseUrlPath = mempty
