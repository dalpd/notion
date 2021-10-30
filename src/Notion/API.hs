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
type NotionAPI = Databases

type Databases = "databases" :> RetrieveDatabase

type RetrieveDatabase = AuthorizationHeader :> VersionHeader :> Capture "database_id" DatabaseId :> Get '[JSON] Database

------------------------------------------------------------------------------
notionAPI :: Proxy NotionAPI
notionAPI = Proxy

retrieveDatabase ::
  Maybe Text ->
  Maybe Version ->
  DatabaseId ->
  ClientM Database
retrieveDatabase = client notionAPI
