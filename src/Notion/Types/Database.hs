{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |
module Notion.Types.Database
  ( -- *
    Database (..),
    -- TODO(dalp): Smart constructor for DatabaseId if we're sure they have to
    -- be UUIDs at all times.
    DatabaseId (..),
  )
where

------------------------------------------------------------------------------

import Data.Aeson (FromJSON)
import qualified Data.Aeson as A
import Data.Map.Strict
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Servant.API

------------------------------------------------------------------------------

-- | Type of rich text objects.
data RichTextObjectType
  = RichTextObjectType_Text
  | RichTextObjectType_Mention
  | RichTextObjectType_Equation
  deriving stock Show

instance FromJSON RichTextObjectType where
  parseJSON = \case
    "text" -> pure RichTextObjectType_Text
    "mention" -> pure RichTextObjectType_Mention
    "equation" -> pure RichTextObjectType_Equation
    richTextObjectType ->
      fail $ "Unknown rich text object type" <> show richTextObjectType

------------------------------------------------------------------------------

-- | Type that controls the behavior of the property.
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
  deriving stock Show

instance FromJSON PropertyObjectType where
  parseJSON = \case
    _ -> fail "TODO"
    
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
  deriving stock Show

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
  deriving stock Show

------------------------------------------------------------------------------

-- | Available configuration capabilities for the number property.
newtype NumberConfiguration = NumberConfiguration
  { _numberConfiguration_format :: NumberConfigurationFormat
    -- ^ How the number is displayed in Notion.
  } 
  deriving stock Show

------------------------------------------------------------------------------

-- The following two type synonyms exist only to differentiate between
-- regular and multi select configurations since they're otherwise identical,
-- if in the future select options and multi-select options differentiate we
-- should come back and  give them their own types instead of using
-- `CommonSelectOptions` for both.

-- | Array of `CommonSelectOptions` objects.
type SelectConfiguration = [CommonSelectOptions]

-- | Array of `CommonSelectOptions` objects.
type MultiSelectConfiguration = [CommonSelectOptions]

------------------------------------------------------------------------------

-- | Available options for regular and multi select.
data CommonSelectOptions = CommonSelectOptions
  { _commonSelectOptions_name :: Text,
    -- ^ Name of the option as it appears in Notion.
    _commonSelectOptions_id :: Text,
    -- ^ Identifier of the option, which does not change if the name is
    -- changed. These are sometimes, but not always, UUIDs.
    _commonSelectOptions_color :: SelectOptionColor
    -- ^ Color of the option. 
  }
  deriving stock Show
------------------------------------------------------------------------------

-- | TODO(dalp): A type to represent valid formulas accepted by Notion.
-- https://www.notion.so/Formulas-28f3f5c3ae644c59b4d862046ea6a541
newtype FormulaConfiguration = FormulaConfiguration
  { _formulaConfiguration_expression :: Text
    -- ^ Formula to evaluate for this property.
  }
  deriving stock Show
------------------------------------------------------------------------------

-- | Available configuration capabilities for the relation property.
data RelationConfiguration = RelationConfiguration
  { _relationConfiguration_databaseId  :: UUID,
    -- ^ The database this relation refers to. New linked pages must belong to
    -- this database in order to be valid.
    _relationConfiguration_syncedPropertyName :: Maybe Text,
    -- ^ By default, relations are formed as two synced properties across
    -- databases: if you make a change to one property, it updates the synced
    -- property at the same time. `_relationConfiguration_syncedPropertyName`
    -- refers to the name of the property in the related database.
    _relationConfiguration_syncedPropertyId :: Maybe Text
    -- ^ By default, relations are formed as two synced properties across
    -- databases: if you make a change to one property, it updates the synced
    -- property at the same time. `_relationConfiguration_syncedPropertyId`
    -- refers to the id of the property in the related database.
    -- This is usually a short string of random letters and symbols.
  }
  deriving stock Show

------------------------------------------------------------------------------

-- | Available configuration capabilities for the rollup property.
data RollupConfiguration = RollupConfiguration
  { _rollupConfiguration_relationPropertyName :: Text,
    -- ^ The name of the relation property this property is responsible for
    -- rolling up.
    _rollupConfiguration_relationPropertyId :: Text,
    -- ^ The id of the relation property this property is responsible for
    -- rolling up.
    _rollupConfiguration_rollupPropertyName :: Text,
    -- ^ The name of the property of the pages in the related database that is
    -- used as an input to function.
    _rollupConfiguration_rollupPropertyId :: Text,
    -- ^ The id of the property of the pages in the related database that is
    -- used as an input to function.
    _rollupConfiguration_function :: RollupFunction
    -- ^ The function that is evaluated for every page in the relation of the
    -- rollup.
  }
  deriving stock Show              
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
    
------------------------------------------------------------------------------

-- | Style information which applies to the whole rich text object.
data Annotations = Annotations
  { _annotations_bold :: Bool,
    -- ^ Whether the text is bolded.
    _annotations_italic :: Bool,
    -- ^ Whether the text is italicized.
    _annotations_strikethrough :: Bool,
    -- ^ Whether the text is struck through.
    _annotations_underline :: Bool,
    -- ^ Whether the text is underlined.
    _annotations_code :: Bool,
    -- ^ Whether the text is of code style.
    _annotations_color :: AnnotationColor
    -- ^ Color of the text. 
  }
  deriving stock Show
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
    

------------------------------------------------------------------------------

-- | Metadata that controls how a database property behaves.
data PropertyObject = PropertyObject
  { _propertyObject_id :: Text,
    -- ^ The ID of the property, usually a short string of random letters and
    -- symbols. Some automatically generated property types have special
    -- human-readable IDs.
    _propertyObject_type :: PropertyObjectType
    -- ^ Type that controls the behavior of the property.
  }
  deriving stock Show

instance FromJSON PropertyObject where
  parseJSON = A.withObject "PropertyObject" $ \o -> do
    _propertyObject_id <- o A..: "id"
    _propertyObject_type <- o A..: "type"
    pure $
      PropertyObject {..}
      
------------------------------------------------------------------------------

-- | Unique identifier for the database.
newtype DatabaseId = DatabaseId { _databaseId :: UUID }
  deriving newtype Show

-- TODO(dalp): Maybe just newtype derive?
instance FromJSON DatabaseId where
  parseJSON = A.withObject "DatabaseId" $ \o -> do
    _databaseId <- o A..: "id"
    pure $
      DatabaseId {..}

instance ToHttpApiData DatabaseId where
  toUrlPiece DatabaseId {..} = UUID.toText _databaseId

------------------------------------------------------------------------------

-- | Database objects describe the property schema of a database in Notion.
-- Pages are the items (or children) in a database. Page property values must
-- conform to the property objects laid out in the parent database object.
data Database = Database
  { _database_object :: Text,
    -- ^ Always "database".
    _database_id :: DatabaseId,
    -- ^ Unique identifier for the database.
    _database_createdTime :: UTCTime,
    -- ^ Date and time when this database was created.
    -- Formatted as an ISO 8601 date time string.
    _database_lastEditedTime :: UTCTime,
    -- ^ Date and time when this database was updated.
    -- Formatted as an ISO 8601 date time string.
    _database_title :: [RichTextObject],
    -- ^ Name of the database as it appears in Notion.
    _database_properties :: Map Text PropertyObject
    -- ^ Schema of properties for the database as they appear in Notion.   
  }
  deriving stock Show

instance FromJSON Database where
  parseJSON = A.withObject "Database" $ \o -> do
    _database_object <- o A..: "object"
    _database_id <- o A..: "id"
    _database_createdTime <- o A..: "created_time"
    _database_lastEditedTime <- o A..: "last_edited_time"
    _database_title <- o A..: "title"
    _database_properties <- o A..: "properties"
    pure $
      Database {..}
