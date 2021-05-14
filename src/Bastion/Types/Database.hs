module Bastion.Types.Database
  ( -- *
    Database (..),
    DatabaseId,
  )
where

------------------------------------------------------------------------------

import Data.Map.Strict
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import URI.ByteString (URI)

------------------------------------------------------------------------------

-- |
data RichTextObjectType
  = RichTextObjectType_Text
  | RichTextObjectType_Mention
  | RichTextObjectType_Equation

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

newtype NumberConfiguration = NumberConfiguration
  { _numberConfiguration_format :: NumberConfigurationFormat
  -- ^ How the number is displayed in Notion.
  } 

type SelectConfiguration = [CommonSelectOptions]

type MultiSelectConfiguration = [CommonSelectOptions]

data CommonSelectOptions = CommonSelectOptions
  { _commonSelectOptions_name :: Text,
    -- ^ Name of the option as it appears in Notion.
    _commonSelectOptions_id :: Text,
    -- ^ Identifier of the option, which does not change if the name is
    -- changed. These are sometimes, but not always, UUIDs.
    _commonSelectOptions_color :: SelectOptionColor
    -- ^ Color of the option. 
  }

newtype FormulaConfiguration = FormulaConfiguration
  { _formulaConfiguration_expression :: Text
    -- ^ Formula to evaluate for this property.
    -- TODO(dalp): A type to represent valid formulas accepted by Notion.
    -- https://www.notion.so/Formulas-28f3f5c3ae644c59b4d862046ea6a541
  }

data PropertyObjectType
  = PropertyObjectType_Title
  | PropertyObjectType_RichText
  | PropertyObjectType_Number NumberConfiguration
  | PropertyObjectType_Select SelectConfiguration
  | PropertyObjectType_MultiSelect MultiSelectConfiguration
  | PropertyObjectType_Date
  | PropertyObjectType_People
  | PropertyObjectType_File
  | PropertyObjectType_Checkbox
  | PropertyObjectType_Url
  | PropertyObjectType_Email
  | PropertyObjectType_PhoneNumber
  | PropertyObjectType_Formula FormulaConfiguration
  | PropertyObjectType_Relation RelationConfiguration
  | PropertyObjectType_Rollup RollupConfiguration
  | PropertyObjectType_CreatedTime
  | PropertyObjectType_CreatedBy
  | PropertyObjectType_LastEditedTime
  | PropertyObjectType_LastEditedBy
              
------------------------------------------------------------------------------

-- |
data Color
  = Color_Default
  | Color_Gray
  | Color_Brown
  | Color_Orange
  | Color_Yellow
  | Color_Green
  | Color_Blue
  | Color_Purple
  | Color_Pink
  | Color_Red
  | Color_GrayBackground
  | Color_BrownBackground
  | Color_OrangeBackground
  | Color_YellowBackground
  | Color_GreenBackground
  | Color_BlueBackground
  | Color_PurpleBackground
  | Color_PinkBackground
  | Color_RedBackground

------------------------------------------------------------------------------

-- |
data Annotations = Annotations
  { _annotations_bold :: Bool,
    _annotations_italic :: Bool,
    _annotations_strikethrough :: Bool,
    _annotations_underline :: Bool,
    _annotations_code :: Bool,
    _annotations_color :: Color
  }

------------------------------------------------------------------------------

-- |
data RichTextObject = RichTextObject
  { _richTextObject_plainText :: Text,
    -- ^ The plain text without annotations.
    _richTextObject_href :: Maybe URI,
    -- ^ The URL of any link or internal Notion mention in this text, if any.
    _richTextObject_annotations :: Annotations,
    -- ^ All annotations that apply to this rich text.
    -- Annotations include colors and bold/italics/underline/strikethrough.
    _richTextObject_type :: RichTextObjectType
  }

data PropertyObject = PropertyObject
  { _propertyObject_id :: Text,
    _propertyObject_type :: PropertyObjectType
  }

------------------------------------------------------------------------------

-- |
newtype DatabaseId = DatabaseId { _databaseId :: UUID}

------------------------------------------------------------------------------

-- |
data Database = Database
  { _database_object :: Text,
    _database_id :: DatabaseId,
    _database_createdTime :: UTCTime,
    _database_lastEditedTime :: UTCTime,
    _database_title :: [RichTextObject],
    _database_properties :: Map Text PropertyObject
  }
