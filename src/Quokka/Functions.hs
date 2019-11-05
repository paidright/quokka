-- |
-- Module      :  Quokka.Functions
-- Copyright   :  © 2019 Shirren Premaratne
-- License     :  MIT
--
-- Maintainer  :  Shirren Premaratne <shirren.premaratne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- Functions to generate Postgres data via the postgres-simple library.

{-# LANGUAGE OverloadedStrings #-}

module Quokka.Functions (
  build
, build1
, buildWith1Rel
, build1With1Rel
, buildWithManyRels
, build1WithManyRels
, delete
, deleteStatement
, id'
, insertStatement
, insertStatementWith1Rel
, insertStatementWithManyRels
, mapFromIdToResult
) where

import Data.Functor (void)
import Data.Int (Int64)
import Data.Text (intercalate)
import Data.Text.Encoding (encodeUtf8)
import Database.PostgreSQL.Simple (Connection, ToRow, execute_, returning, query)
import Database.PostgreSQL.Simple.Types (Query (Query))
import Quokka.Types (ChildTable (ChildTable)
                    , Id (getId)
                    , ParentTable (ParentTable)
                    , Table (Table)
                    , Result (SingleResult))
import Quokka.Text.Countable (singularize)


-- | Build a prepared statement to insert data into the database`
build
  :: (ToRow q)
  => Connection
  -> ParentTable
  -> [q]
  -> IO [Id]
build conn tbl =
  let
    qry = insertStatement tbl
  in
  returning conn qry


-- | Similar to the build function but we only ever return
-- a single optional result, and only take 1 value.
build1
  :: (ToRow q)
  => Connection
  -> ParentTable
  -> q
  -> IO (Maybe Id)
build1 conn tbl =
  let
    qry = insertStatement tbl
  in
  fmap build1Helper . query conn qry


-- | Build a prepared statement for a child table with a single foreign
-- key table
buildWith1Rel
  :: (ToRow q)
  => Connection
  -> ParentTable
  -> ChildTable
  -> [q]
  -> IO [Id]
buildWith1Rel conn parent child =
  let
    qry = insertStatementWith1Rel parent child
  in
  returning conn qry


-- | Build a prepared statement for a child table with a single foreign
-- key table
build1With1Rel
  :: (ToRow q)
  => Connection
  -> ParentTable
  -> ChildTable
  -> q
  -> IO (Maybe Id)
build1With1Rel conn parent child =
  let
    qry = insertStatementWith1Rel parent child
  in
  fmap build1Helper . query conn qry


-- | Build a prepared statement for a child table with more than 1 parent
buildWithManyRels
  :: (ToRow q)
  => Connection
  -> [ParentTable]
  -> ChildTable
  -> [q]
  -> IO [Id]
buildWithManyRels conn parents child =
  let
    qry = insertStatementWithManyRels parents child
  in
  returning conn qry


-- | Build a prepared statement for a child table with more than 1 parent
build1WithManyRels
  :: (ToRow q)
  => Connection
  -> [ParentTable]
  -> ChildTable
  -> q
  -> IO (Maybe Id)
build1WithManyRels conn parents child =
  let
    qry = insertStatementWithManyRels parents child
  in
  fmap build1Helper . query conn qry


-- | Perform a truncate with cascade action on the Table
delete
  :: Connection
  -> Table
  -> IO Int64
delete conn tbl = do
  let
    alter = alterSequenceStatement tbl
    qry   = deleteStatement tbl
  void $ execute_ conn alter
  execute_ conn qry


-- | Create an insert statement for a table
insertStatement
  :: ParentTable
  -> Query
insertStatement (ParentTable name columns) =
  let
    columnsAsText = intercalate "," columns
    valuesAsText  = intercalate "," (map (const "?") columns)
    baseInsert    = "insert into " <> name <> " (" <> columnsAsText <> ")"
  in
  Query (encodeUtf8 $ baseInsert <> " values (" <> valuesAsText <> ") returning id;")


-- | Creates an insert statement for a table, and uses the parent table to also incude
-- a foreign key in the generation of the statement.
insertStatementWith1Rel
  :: ParentTable
  -> ChildTable
  -> Query
insertStatementWith1Rel parent =
  insertStatementWithManyRels [parent]


-- | Creates an insert statement for a table, and uses multiple parent tables to also include
-- foreign keys in the generation of the statement.
insertStatementWithManyRels
  :: [ParentTable]
  -> ChildTable
  -> Query
insertStatementWithManyRels parents (ChildTable name columns) =
  let
    updatedColumns = columns ++ map (\(ParentTable parentName _) -> singularize parentName <> "_id") parents
    columnsAsText = intercalate "," updatedColumns
    valuesAsText  = intercalate "," (map (const "?") updatedColumns)
    baseInsert    = "insert into " <> name <> " (" <> columnsAsText <> ")"
  in
  Query (encodeUtf8 $ baseInsert <> " values (" <> valuesAsText <> ") returning id;")


-- | Generate an alter sequence statement for a table
alterSequenceStatement
  :: Table
  -> Query
alterSequenceStatement (Table name) =
  Query (encodeUtf8 $ "alter sequence " <> name <> "_id_seq restart;")


-- | Generate a delete statement for a table
deleteStatement
  :: Table
  -> Query
deleteStatement (Table name) =
  Query (encodeUtf8 $ "truncate table " <> name <> " cascade;")


-- | Helper function to extract the underlying Int value from
-- the first value in the list
id' :: [Id] -> Int
id' (x:_) =
  getId x
id' [] =
  -1

-- | Postgres Simple does not have a function which maps from [r] -> Maybe r
-- so we've written one that takes the head or returns Nothing in a safe
-- manner.
build1Helper
  :: [Id]
  -> Maybe Id
build1Helper (x:_) =
  Just x
build1Helper [] =
  Nothing


-- | Function to map from IO [Id] -> IO [Result]
mapFromIdToResult
  :: ParentTable
  -> [Id]
  -> [Result]
mapFromIdToResult tbl =
  map (SingleResult tbl)
