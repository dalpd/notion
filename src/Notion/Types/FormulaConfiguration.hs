-- |
module Notion.Types.FormulaConfiguration
  ( -- * Type definitions
    FormulaConfiguration (..),
  )
where

------------------------------------------------------------------------------

import Data.Text (Text)

------------------------------------------------------------------------------

-- | TODO(dalp): A type to represent valid formulas accepted by Notion.
-- https://www.notion.so/Formulas-28f3f5c3ae644c59b4d862046ea6a541
newtype FormulaConfiguration = FormulaConfiguration
  { -- | Formula to evaluate for this property.
    _formulaConfiguration_expression :: Text
  }
  deriving stock (Show)

------------------------------------------------------------------------------
