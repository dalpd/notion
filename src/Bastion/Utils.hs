-- | Shared utilities, things like type synonyms for headers and params.
module Bastion.Utils
  ( -- *
    AuthorizationHeader,
    VersionHeader,

    -- *
    GetJSONTyped,
  )
where

------------------------------------------------------------------------------

import Data.Text (Text)
import Servant.API

------------------------------------------------------------------------------

-- | Type synonym for the HTTP "Authorization" header used to both
-- authenticate and authorize operations. The Notion API accepts bearer tokens
-- in this header.
type AuthorizationHeader = Header "Authorization" Text

------------------------------------------------------------------------------

-- |
data Version = Version_V1

-- |
type VersionHeader = Header "Notion-Version" Version

------------------------------------------------------------------------------

-- | Type synonym for GET operations returning JSON that can be parsed into
-- `returnType`.
type GetJSONTyped returnType = Get '[JSON] returnType
