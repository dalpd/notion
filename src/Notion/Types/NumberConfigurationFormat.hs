-- |
module Notion.Types.NumberConfigurationFormat
  ( -- * Type definitions
    NumberConfigurationFormat (..),
  )
where

------------------------------------------------------------------------------

-- | How the numbers are displayed in Notion.
data NumberConfigurationFormat
  = NumberConfigurationFormat_Number
  | NumberConfigurationFormat_NumberWithCommas
  | NumberConfigurationFormat_Percent
  | NumberConfigurationFormat_Dollar
  | NumberConfigurationFormat_Euro
  | NumberConfigurationFormat_Pound
  | NumberConfigurationFormat_Yen
  | NumberConfigurationFormat_Ruble
  | NumberConfigurationFormat_Rupee
  | NumberConfigurationFormat_Won
  | NumberConfigurationFormat_Yuan
  deriving stock (Show)
