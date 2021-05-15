-- | Shared types for the <https://developers.notion.com Notion API> wrapper
-- <github.com/dalpd/bastion Bastion>.
--
-- https://docs.servant.dev/en/stable/cookbook/structuring-apis/StructuringApis.html
module Bastion.Types
  ( module API,
    module DB,
  )
where

------------------------------------------------------------------------------

import Bastion.Types.API as API
import Bastion.Types.Database as DB

------------------------------------------------------------------------------
