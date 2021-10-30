-- |
module Notion.Types.Annotations
  ( -- * Type definitions
    Annotations (..),
  )
where

------------------------------------------------------------------------------

import Data.Aeson (FromJSON)
import qualified Data.Aeson as A
import Notion.Types.AnnotationColor

------------------------------------------------------------------------------

-- | Style information which applies to the whole rich text object.
data Annotations = Annotations
  { -- | Whether the text is bolded.
    _annotations_bold :: Bool,
    -- | Whether the text is italicized.
    _annotations_italic :: Bool,
    -- | Whether the text is struck through.
    _annotations_strikethrough :: Bool,
    -- | Whether the text is underlined.
    _annotations_underline :: Bool,
    -- | Whether the text is of code style.
    _annotations_code :: Bool,
    -- | Color of the text.
    _annotations_color :: AnnotationColor
  }
  deriving stock (Show)

instance FromJSON Annotations where
  parseJSON = A.withObject "Annotations" $ \o -> do
    _annotations_bold <- o A..: "bold"
    _annotations_italic <- o A..: "italic"
    _annotations_strikethrough <- o A..: "striketrough"
    _annotations_underline <- o A..: "underline"
    _annotations_code <- o A..: "code"
    _annotations_color <- o A..: "color"
    pure $
      Annotations {..}
