-- |
module Notion.Types.CommonSelectOptions
  ( -- * Type definitions
    CommonSelectOptions (..),
  )
where

------------------------------------------------------------------------------

import Notion.Prelude
import Notion.Types.SelectOptionColor

------------------------------------------------------------------------------

-- | Available options for regular and multi select.
data CommonSelectOptions = CommonSelectOptions
  { -- | Name of the option as it appears in Notion.
    _commonSelectOptions_name :: Text,
    -- | Identifier of the option, which does not change if the name is
    -- changed. These are sometimes, but not always, UUIDs.
    _commonSelectOptions_id :: Text,
    -- | Color of the option.
    _commonSelectOptions_color :: SelectOptionColor
  }
  deriving stock (Show)
