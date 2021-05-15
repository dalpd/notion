{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
-- |
module Bastion
  ( main
  )
where

------------------------------------------------------------------------------

import Bastion.Types
import Bastion.Client as Client
import Bastion.Utils as Utils
import Data.UUID (fromText)

------------------------------------------------------------------------------

main :: IO ()
main = do
  key <- Utils.accessKey "NOTION_API_KEY"
  databaseId <- fromText <$> Utils.accessKey "DATABASE_ID" >>= \case
    Nothing -> fail "Database id isn't valid UUID"
    Just uuid -> pure uuid
  result <-
    Client.retrieveDatabase
      (Just key)
      (Just Version_V1)
      (DatabaseId $ databaseId)
  case result of
    Left err -> fail $ "Retrieval is unsuccesful: " <> show err
    Right database -> print database
