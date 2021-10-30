-- | API representation of the <https://developers.notion.com Notion API>.
module Notion.API
  ( retrieveDatabase,
  )
where

------------------------------------------------------------------------------

import Data.Proxy
import Notion.Prelude
import Notion.Types (DatabaseId)
import Notion.Types.Database
import Notion.Utils
import Servant.API
import Servant.Client (ClientM, client)

------------------------------------------------------------------------------
type NotionAPI
  = AuthorizationHeader
  :> VersionHeader
  :> Databases

type Databases = "databases" :> RetrieveDatabase

type RetrieveDatabase
  = Capture "database_id" DatabaseId
  :> Get '[JSON] Database

-- TODO(dalp): Update response type to be a paginated result.
type Search = "search" :> Text

------------------------------------------------------------------------------
notionAPI :: Proxy NotionAPI
notionAPI = Proxy

retrieveDatabase ::
  Maybe Text ->
  Maybe Version ->
  DatabaseId ->
  ClientM Database
retrieveDatabase = client notionAPI
