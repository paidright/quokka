{-# LANGUAGE OverloadedStrings #-}

module Quokka.Types (
  Data
, Id (..)
, Table (..)
, Result (..)
) where

import Data.Text (Text)
import Database.PostgreSQL.Simple (FromRow)
import Database.PostgreSQL.Simple.FromRow (fromRow, field)


type Data = [Text]


data Id
  = Id { getId :: Int }


instance FromRow Id where
  fromRow =
    Id <$> field


data Table
  = Table Text [Text]
--  | TableWithParent Text [Text] Table
--  | TableWithMultipleParents Text [Text] [Table]


data Result
  = Result Table [Id]
