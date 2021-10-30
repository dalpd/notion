-- |
module Notion.Types.PropertyObjectType
  ( -- * Type definitions
    PropertyObjectType (..),
  )
where

------------------------------------------------------------------------------

import qualified Data.Aeson as A
import Notion.Prelude
import Notion.Types.CommonSelectOptions
import Notion.Types.FormulaConfiguration
import Notion.Types.NumberConfiguration
import Notion.Types.RelationConfiguration
import Notion.Types.RollupConfiguration

------------------------------------------------------------------------------

-- | Type that controls the behavior of the property.
data PropertyObjectType
  = PropertyObjectType_Title
  | PropertyObjectType_RichText
  | PropertyObjectType_Number NumberConfiguration
  | PropertyObjectType_Select SelectConfiguration
  | PropertyObjectType_MultiSelect MultiSelectConfiguration
  | PropertyObjectType_Date
  | PropertyObjectType_People
  | PropertyObjectType_File
  | PropertyObjectType_Checkbox
  | PropertyObjectType_Url
  | PropertyObjectType_Email
  | PropertyObjectType_PhoneNumber
  | PropertyObjectType_Formula FormulaConfiguration
  | PropertyObjectType_Relation RelationConfiguration
  | PropertyObjectType_Rollup RollupConfiguration
  | PropertyObjectType_CreatedTime
  | PropertyObjectType_CreatedBy
  | PropertyObjectType_LastEditedTime
  | PropertyObjectType_LastEditedBy
  deriving stock (Show)

instance FromJSON PropertyObjectType where
  parseJSON = \case
    _ -> fail "TODO"

------------------------------------------------------------------------------
-- The following two type synonyms exist only to differentiate between
-- regular and multi select configurations since they're otherwise identical,
-- if in the future select options and multi-select options differentiate we
-- should come back and  give them their own types instead of using
-- `CommonSelectOptions` for both.

-- | Array of `CommonSelectOptions` objects.
type SelectConfiguration = [CommonSelectOptions]

-- | Array of `CommonSelectOptions` objects.
type MultiSelectConfiguration = [CommonSelectOptions]
