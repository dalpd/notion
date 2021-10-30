-- |
module Notion.Types.AnnotationColor
  ( -- * Type definitions
    AnnotationColor (..),
  )
where

------------------------------------------------------------------------------

import Data.Aeson (FromJSON)
import qualified Data.Aeson as A

------------------------------------------------------------------------------

-- | Color of the text used in `Annotations`.
data AnnotationColor
  = AnnotationColor_Default
  | AnnotationColor_Gray
  | AnnotationColor_Brown
  | AnnotationColor_Orange
  | AnnotationColor_Yellow
  | AnnotationColor_Green
  | AnnotationColor_Blue
  | AnnotationColor_Purple
  | AnnotationColor_Pink
  | AnnotationColor_Red
  | AnnotationColor_GrayBackground
  | AnnotationColor_BrownBackground
  | AnnotationColor_OrangeBackground
  | AnnotationColor_YellowBackground
  | AnnotationColor_GreenBackground
  | AnnotationColor_BlueBackground
  | AnnotationColor_PurpleBackground
  | AnnotationColor_PinkBackground
  | AnnotationColor_RedBackground
  deriving stock Show
-- TODO(dalp): Add `autoPrism`, `parseJSONWithPrism`, `toJSONWithPrism`
-- helpers and define Aeson instances for sum types in terms of those.
instance FromJSON AnnotationColor where
  parseJSON = pure . \case
    "default" -> AnnotationColor_Default
    "gray" -> AnnotationColor_Gray
    "brown" -> AnnotationColor_Brown
    "orange" -> AnnotationColor_Orange
    "yellow" -> AnnotationColor_Yellow
    "green" -> AnnotationColor_Green
    "blue" -> AnnotationColor_Blue
    "purple" -> AnnotationColor_Purple
    "pink" -> AnnotationColor_Pink
    "red" -> AnnotationColor_Red
    "gray_background" -> AnnotationColor_GrayBackground
    "brown_background" -> AnnotationColor_BrownBackground
    "orange_background" -> AnnotationColor_OrangeBackground
    "yellow_background" -> AnnotationColor_YellowBackground
    "green_background" -> AnnotationColor_GreenBackground
    "blue_background" -> AnnotationColor_BlueBackground
    "purple_background" -> AnnotationColor_PurpleBackground
    "pink_background" -> AnnotationColor_PinkBackground
    "red_background" -> AnnotationColor_RedBackground
    _ -> AnnotationColor_Default
