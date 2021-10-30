-- |
module Notion.Types.DatabaseId
  ( -- * Type definitions
    DatabaseId (..),
  )
where

------------------------------------------------------------------------------

import Data.Aeson (FromJSON)
import qualified Data.Aeson as A
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Servant.API

------------------------------------------------------------------------------

-- | Newtype wrapper over `UUID` to represent unique identifiers for
-- databases.
newtype DatabaseId = DatabaseId {_databaseId :: UUID}
  deriving newtype (Show)

instance FromJSON DatabaseId where
  parseJSON = A.withObject "DatabaseId" $ \o -> do
    _databaseId <- o A..: "id"
    pure $ DatabaseId {..}

instance ToHttpApiData DatabaseId where
  toUrlPiece DatabaseId {..} = UUID.toText _databaseId
