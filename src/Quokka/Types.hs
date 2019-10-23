module Quokka.Types (
  ChildTable (..)
, Data
, Id (..)
, ParentTable (..)
, Table (..)
, Result (..)
, Row (..)
) where

import Data.Text (Text)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Simple.FromRow (fromRow, field)
import Database.PostgreSQL.Simple.ToRow (toRow)
import Database.PostgreSQL.Simple.ToField (toField, ToField)


type Data = [Text]


newtype Id
  = Id { getId :: Int }


instance FromRow Id where
  fromRow =
    Id <$> field


data ChildTable
  = ChildTable Text [Text]


data ParentTable
  = ParentTable Text [Text]


newtype Table
  = Table Text

data Result
  = SingleResult ParentTable Id


newtype Row a
  = Row [a]

instance ToField a => ToRow (Row a) where
  toRow (Row xs) =
    map toField xs