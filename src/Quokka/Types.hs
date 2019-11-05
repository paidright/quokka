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
, FK (..)
, Id (..)
, ParentTable (..)
, Table (..)
, Relation (..)
, Result (..)
, Row (..)
) where

import Data.Text (Text)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Simple.FromRow (fromRow, field)
import Database.PostgreSQL.Simple.ToRow (toRow)
import Database.PostgreSQL.Simple.ToField (toField, ToField)


-- | Alias for a list of values.
type Data = [Text]


-- | Represents the identity column of a row in a table. I.e. the
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


-- | A child table represents a relation in Postgres with a foreign key
-- to a parent table.
data ChildTable
  = ChildTable Text [Text]


-- | A parent table represents a relation in Postgres with no foreign keys.
data ParentTable
  = ParentTable Text [Text]


-- | This type is used to model and create deletes.
newtype Table
  = Table Text


-- | A relation is defined as two tables with a relationship
-- to one another. The relationship is mapped through a foreign
-- key (FK).
data Relation
  = Relation ParentTable FK


-- | Column that represents a foreign key in a database.
newtype FK
  = FK Text


-- | Represents a result retrieved by Quokka via Postgres-simple.
data Result
  = SingleResult ParentTable Id


-- | A row represents a Postgres row retrieved for a relation.
newtype Row a
  = Row [a]

instance ToField a => ToRow (Row a) where
  toRow (Row xs) =
    map toField xs
