-- |
module Notion.Types.RichTextObject
  ( -- * Type definitions
    RichTextObject (..),
  )
where

------------------------------------------------------------------------------

import Data.Aeson (FromJSON)
import qualified Data.Aeson as A
import Data.Text (Text)
import Notion.Types.RichTextObjectType
import Notion.Types.Annotations

------------------------------------------------------------------------------

-- | Rich text objects contain data for displaying formatted text, mentions,
-- and equations. A rich text object also contains annotations for style
-- information.
-- Arrays of rich text objectss are used within property objects and property
-- value objects to create what a user sees as a single text value in Notion.
data RichTextObject = RichTextObject
  { _richTextObject_plainText :: Text,
    -- ^ The plain text without annotations.
    _richTextObject_href :: Maybe Text, -- TODO(dalp): Use uri-bytestring's URI
    -- ^ The URL of any link or internal Notion mention in this text, if any.
    _richTextObject_annotations :: Annotations,
    -- ^ All annotations that apply to this rich text.
    -- Annotations include colors and bold/italics/underline/strikethrough.
    _richTextObject_type :: RichTextObjectType
  }
  deriving stock Show

instance FromJSON RichTextObject where
  parseJSON = A.withObject "RichTextObject" $ \o -> do
    _richTextObject_plainText <- o A..: "plain_text"
    _richTextObject_href <- o A..:? "href"
    _richTextObject_annotations <- o A..: "annotations"
    _richTextObject_type <- o A..: "type"
    pure $
      RichTextObject {..}
