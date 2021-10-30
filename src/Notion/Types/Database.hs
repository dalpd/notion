-- |
module Notion.Types.Database
  ( -- *
    Database (..),
    DatabaseId (..),
  )
where

------------------------------------------------------------------------------

import Data.Aeson (FromJSON)
import qualified Data.Aeson as A
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Time (UTCTime)
import Notion.Types.RichTextObject
import Notion.Types.PropertyObject
import Notion.Types.DatabaseId

------------------------------------------------------------------------------

-- | Database objects describe the property schema of a database in Notion.
-- Pages are the items (or children) in a database. Page property values must
-- conform to the property objects laid out in the parent database object.
data Database = Database
  { _database_object :: Text,
    -- ^ Always "database".
    _database_id :: DatabaseId,
    -- ^ Unique identifier for the database.
    _database_createdTime :: UTCTime,
    -- ^ Date and time when this database was created.
    -- Formatted as an ISO 8601 date time string.
    _database_lastEditedTime :: UTCTime,
    -- ^ Date and time when this database was updated.
    -- Formatted as an ISO 8601 date time string.
    _database_title :: [RichTextObject],
    -- ^ Name of the database as it appears in Notion.
    _database_properties :: Map Text PropertyObject
    -- ^ Schema of properties for the database as they appear in Notion.   
  }
  deriving stock Show

instance FromJSON Database where
  parseJSON = A.withObject "Database" $ \o -> do
    _database_object <- o A..: "object"
    _database_id <- o A..: "id"
    _database_createdTime <- o A..: "created_time"
    _database_lastEditedTime <- o A..: "last_edited_time"
    _database_title <- o A..: "title"
    _database_properties <- o A..: "properties"
    pure $
      Database {..}
