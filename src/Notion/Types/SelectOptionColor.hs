-- |
module Notion.Types.SelectOptionColor
  ( -- * Type definitions
    SelectOptionColor (..),
  )
where

------------------------------------------------------------------------------

-- | Color of the option.
data SelectOptionColor
  = SelectOptionColor_Default
  | SelectOptionColor_Gray
  | SelectOptionColor_Brown
  | SelectOptionColor_Orange
  | SelectOptionColor_Yellow
  | SelectOptionColor_Green
  | SelectOptionColor_Blue
  | SelectOptionColor_Purple
  | SelectOptionColor_Pink
  | SelectOptionColor_Red
  deriving stock Show
