-- | Shared utilities, things like type synonyms for headers and params.
module Notion.Utils
  ( -- * Type definitions/synonmys
    AuthorizationHeader,
    VersionHeader,
    Version (..),

    -- * Utility functions
    accessKey,
  )
where

------------------------------------------------------------------------------

import qualified Data.Text as T
import LoadEnv
import Notion.Prelude
import Servant.API
import System.Environment

------------------------------------------------------------------------------

-- | Type synonym for the HTTP "Authorization" header used to both
-- authenticate and authorize operations. The Notion API accepts bearer tokens
-- in this header.
type AuthorizationHeader = Header "Authorization" Text

------------------------------------------------------------------------------

-- |
data Version = Version_V1

instance ToHttpApiData Version where
  toUrlPiece Version_V1 = "v1"

------------------------------------------------------------------------------

-- |
type VersionHeader = Header "Notion-Version" Version

------------------------------------------------------------------------------

-- | A utility function to lookup a certain key in the .env file in the
-- directory you're calling this function from. If you get a hit `accessKey`
-- will return the associated value, and if not you get an error.
accessKey :: String -> IO Text
accessKey key =
  loadEnv >> lookupEnv key >>= \case
    Just k -> pure $ T.pack k
    Nothing -> error $ errorMsg key
  where
    errorMsg k = "Couldn't find `" <> k <> "` in .env"
