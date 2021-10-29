-- | Shared types for the <https://developers.notion.com Notion API> wrapper
-- <github.com/dalpd/notion Notion>.
--
-- https://docs.servant.dev/en/stable/cookbook/structuring-apis/StructuringApis.html
module Notion.Types
  ( module API,
    module DB,
  )
where

------------------------------------------------------------------------------

import Notion.Types.API as API
import Notion.Types.Database as DB

------------------------------------------------------------------------------
