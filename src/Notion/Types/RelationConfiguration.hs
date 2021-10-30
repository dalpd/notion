-- |
module Notion.Types.RelationConfiguration
  ( -- * Type definitions
    RelationConfiguration (..),
  )
where

------------------------------------------------------------------------------

import Data.UUID (UUID)
import Data.Text (Text)

------------------------------------------------------------------------------

-- | Available configuration capabilities for the relation property.
data RelationConfiguration = RelationConfiguration
  { _relationConfiguration_databaseId  :: UUID,
    -- ^ The database this relation refers to. New linked pages must belong to
    -- this database in order to be valid.
    _relationConfiguration_syncedPropertyName :: Maybe Text,
    -- ^ By default, relations are formed as two synced properties across
    -- databases: if you make a change to one property, it updates the synced
    -- property at the same time. `_relationConfiguration_syncedPropertyName`
    -- refers to the name of the property in the related database.
    _relationConfiguration_syncedPropertyId :: Maybe Text
    -- ^ By default, relations are formed as two synced properties across
    -- databases: if you make a change to one property, it updates the synced
    -- property at the same time. `_relationConfiguration_syncedPropertyId`
    -- refers to the id of the property in the related database.
    -- This is usually a short string of random letters and symbols.
  }
  deriving stock Show

------------------------------------------------------------------------------
