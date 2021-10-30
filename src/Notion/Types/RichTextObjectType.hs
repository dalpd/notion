-- |
module Notion.Types.RichTextObjectType
  ( -- * Type definitions
    RichTextObjectType (..),
  )
where

------------------------------------------------------------------------------

import Data.Aeson (FromJSON)
import qualified Data.Aeson as A

------------------------------------------------------------------------------

-- | Type of rich text objects.
data RichTextObjectType
  = RichTextObjectType_Text
  | RichTextObjectType_Mention
  | RichTextObjectType_Equation
  deriving stock (Show)

instance FromJSON RichTextObjectType where
  parseJSON = \case
    "text" -> pure RichTextObjectType_Text
    "mention" -> pure RichTextObjectType_Mention
    "equation" -> pure RichTextObjectType_Equation
    richTextObjectType ->
      fail $ "Unknown rich text object type" <> show richTextObjectType
