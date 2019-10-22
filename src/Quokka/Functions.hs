{-# LANGUAGE OverloadedStrings #-}

module Quokka.Functions (
  create
, createStatement
) where

--import Data.Int (Int64)
import Data.Text (intercalate)
import Data.Text.Encoding (encodeUtf8)
import Database.PostgreSQL.Simple (Connection, ToRow, returning)
import Database.PostgreSQL.Simple.Types (Query (Query))
import Quokka.Types (Table (Table), Id)


create
  :: (ToRow q)
  => Connection
  -> Table
  -> ([q] -> IO [Id])
create conn tbl =
  let
    qry = createStatement tbl
  in
  returning conn qry

  
createStatement
  :: Table
  -> Query 
createStatement (Table name columns) =
  let
    columnsAsText = intercalate "," columns
    valuesAsText  = intercalate "," (map (\_ -> "?") columns)
    baseInsert    = "insert into " <> name <> " (" <> columnsAsText <> ")" 
  in
  Query (encodeUtf8 $ baseInsert <> " values (" <> valuesAsText <> ") returning id;")
