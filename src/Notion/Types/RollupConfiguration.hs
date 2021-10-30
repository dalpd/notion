-- |
module Notion.Types.RollupConfiguration
  ( -- * Type definitions
    RollupConfiguration (..),
  )
where

------------------------------------------------------------------------------

import Data.Text (Text)
import Notion.Types.RollupFunction

------------------------------------------------------------------------------

-- | Available configuration capabilities for the rollup property.
data RollupConfiguration = RollupConfiguration
  { -- | The name of the relation property this property is responsible for
    -- rolling up.
    _rollupConfiguration_relationPropertyName :: Text,
    -- | The id of the relation property this property is responsible for
    -- rolling up.
    _rollupConfiguration_relationPropertyId :: Text,
    -- | The name of the property of the pages in the related database that is
    -- used as an input to function.
    _rollupConfiguration_rollupPropertyName :: Text,
    -- | The id of the property of the pages in the related database that is
    -- used as an input to function.
    _rollupConfiguration_rollupPropertyId :: Text,
    -- | The function that is evaluated for every page in the relation of the
    -- rollup.
    _rollupConfiguration_function :: RollupFunction
  }
  deriving stock (Show)
