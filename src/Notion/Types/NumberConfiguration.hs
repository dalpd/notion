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
  { -- | How the number is displayed in Notion.
    _numberConfiguration_format :: NumberConfigurationFormat
  }
  deriving stock (Show)
