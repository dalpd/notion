-- |
module Notion.Types.NumberConfiguration
  ( -- * Type definitions
    NumberConfiguration (..),
  )
where

------------------------------------------------------------------------------

import Notion.Types.NumberConfigurationFormat

------------------------------------------------------------------------------

-- | Available configuration capabilities for the number property.
newtype NumberConfiguration = NumberConfiguration
  { _numberConfiguration_format :: NumberConfigurationFormat
    -- ^ How the number is displayed in Notion.
  }
  deriving stock Show
