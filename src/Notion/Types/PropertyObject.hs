-- |
module Notion.Types.PropertyObject
  ( -- * Type definitions
    PropertyObject (..),
  )
where

------------------------------------------------------------------------------

import Data.Aeson (FromJSON)
import qualified Data.Aeson as A
import Data.Text (Text)
import Notion.Types.PropertyObjectType

------------------------------------------------------------------------------

-- | Metadata that controls how a database property behaves.
data PropertyObject = PropertyObject
  { _propertyObject_id :: Text,
    -- ^ The ID of the property, usually a short string of random letters and
    -- symbols. Some automatically generated property types have special
    -- human-readable IDs.
    _propertyObject_type :: PropertyObjectType
    -- ^ Type that controls the behavior of the property.
  }
  deriving stock Show

instance FromJSON PropertyObject where
  parseJSON = A.withObject "PropertyObject" $ \o -> do
    _propertyObject_id <- o A..: "id"
    _propertyObject_type <- o A..: "type"
    pure $
      PropertyObject {..}
