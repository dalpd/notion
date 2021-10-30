-- |
module Notion.Types.RollupFunction
  ( -- * Type definitions
    RollupFunction (..),
  )
where

------------------------------------------------------------------------------

-- | The function that is evaluated for every page in the relation of the
-- rollup.
data RollupFunction
  = RollupFunction_CountAll
  | RollupFunction_CountValues
  | RollupFunction_CountUniqueValues
  | RollupFunction_CountEmpty
  | RollupFunction_CountNotEmpty
  | RollupFunction_PercentEmpty
  | RollupFunction_PercentNotEmpty
  | RollupFunction_Sum
  | RollupFunction_Average
  | RollupFunction_Median
  | RollupFunction_Min
  | RollupFunction_Max
  | RollupFunction_Range
  deriving stock (Show)
