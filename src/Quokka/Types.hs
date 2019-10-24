-- |
-- Module      :  Quokka.Types
-- Copyright   :  © 2019 Shirren Premaratne
-- License     :  MIT
--
-- Maintainer  :  Shirren Premaratne <shirren.premaratne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- Types used by the Functions to generate test data and represent data
-- and relational associations.

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


-- Represents the identity column of a row in a table. I.e. the
-- primary key of a table which is limited to integers.
newtype Id
  = Id { getId :: Int }
      deriving (Eq, Show)

instance FromRow Id where
  fromRow =
    Id <$> field

instance ToField Id where
  toField (Id val) =
    toField val


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