{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Quokka.Helper (
  openConnection
, closeConnection
, withDatabaseConnection
, setupDb
) where

import Control.Exception (bracket)
import Data.Functor (void)
import Data.Int (Int64)
import Database.PostgreSQL.Simple
import Quokka.Functions (delete)
import Quokka.Types (Table (..))
import Text.RawString.QQ


openConnection :: IO Connection
openConnection =
  connect defaultConnectInfo { connectPassword = "quokka_test"
                             , connectDatabase = "quokka_test"
                             , connectUser = "quokka_test"
                             }


closeConnection :: Connection -> IO ()
closeConnection =
  close


withDatabaseConnection :: (Connection -> IO ()) -> IO ()
withDatabaseConnection =
  bracket openConnection closeConnection


setupDb :: IO ()
setupDb = do
  conn <- openConnection
  void $ buildTables conn
  flushDb conn


flushDb :: Connection -> IO ()
flushDb conn = do
  void $ delete conn (Table "users")
  void $ delete conn (Table "accounts")
  void $ delete conn (Table "profiles")
  closeConnection conn


buildTables :: Connection -> IO Int64
buildTables conn = do
  void $ execute_ conn [r|create table if not exists users (
                             id serial primary key
                           , firstname text
                           , lastname text
                           , age integer);|]
  void $ execute_ conn [r|create table if not exists accounts (
                            id serial primary key
                          , name text
                          , description text
                          , user_id integer references users(id));|]
  execute_ conn [r|create table if not exists profiles (
                             id serial primary key
                           , active boolean
                           , user_id integer references users(id)
                           , account_id integer references accounts(id));|]

