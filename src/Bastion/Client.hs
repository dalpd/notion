module Bastion.Client
  ( retrieveDatabase
  )
where

------------------------------------------------------------------------------

import Bastion.Types.Database
import Bastion.Utils
import Data.Text (Text, unpack)
import qualified Bastion.Types.API as API
import Servant.Client (BaseUrl (..), ClientEnv, ClientError, Scheme (Https), mkClientEnv, runClientM)

import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Client (newManager)
import Servant.API (toUrlPiece)

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
