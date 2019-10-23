{-# LANGUAGE OverloadedStrings #-}

module Quokka.CreateSpec (
  spec
) where

import Control.Exception (bracket)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types (Query (Query))
import Database.PostgreSQL.Simple.ToField (toField, ToField)
import Quokka.Types (ChildTable (..), Id (..), ParentTable (..), Table (..), Result (..), Row (..))
import Quokka.Functions (build, buildWith1Rel, delete, insertStatement, insertStatementWith1Rel, mapFromIdToResult)
import Test.Hspec


openConnection :: IO Connection
openConnection =
  connect defaultConnectInfo { connectPassword = "", connectDatabase = "quokka_test" }


closeConnection :: Connection -> IO ()
closeConnection =
  close


withDatabaseConnection :: (Connection -> IO ()) -> IO ()
withDatabaseConnection =
  bracket openConnection closeConnection


flushDb :: IO ()
flushDb = do
  conn <- openConnection
  _    <- delete conn (Table "users")
  _    <- delete conn (Table "accounts")
  closeConnection conn


spec :: Spec
spec = do
  describe "insertStatement" $ do
    it "should return an insert statement for a single column" $ do
      let
        table = ParentTable "users" ["name"]

      insertStatement table `shouldBe` Query "insert into users (name) values (?) returning id;"


    it "should return an insert statement for multiple columns" $ do
      let
        table = ParentTable "users" ["firstname", "lastname"]

      insertStatement table `shouldBe` Query "insert into users (firstname,lastname) values (?,?) returning id;"


  describe "insertStatementWith1Rel" $
    it "should return an insert statement with the FK set" $ do
      let
        parentTable = ParentTable "users" ["firstname", "lastname", "age"]
        childTable  = ChildTable "accounts" ["name"]
        Query stmt  = insertStatementWith1Rel parentTable childTable

      stmt `shouldBe` encodeUtf8 "insert into accounts (name,user_id) values (?,?) returning id;"

  before_ flushDb $
    around withDatabaseConnection $
      describe "insert" $ do
        it "should insert data into the database" $ \conn -> do
          let
            table  = ParentTable "users" ["firstname", "lastname", "age"]
            insert = build conn table
          res  <- insert [
              ("John" :: Text, "Doe" :: Text, Just 1 :: Maybe Int)
            , ("Jane" :: Text, "Doe" :: Text, Nothing)]

          length res `shouldBe` 2


        it "should insert parent and child into the database" $ \conn -> do
          let
            parentTable = ParentTable "users" ["firstname", "lastname", "age"]
            childTable  = ChildTable "accounts" ["name", "account_id"]
            insert      = build conn parentTable
            insertRel   = buildWith1Rel conn parentTable childTable
            pid pids    = getId (head pids)
          pids <- insert [("John" :: Text, "Doe" :: Text, 1 :: Int)]
          cids <- insertRel [("Account-1" :: Text, 2 :: Int, pid pids)]

          length pids `shouldBe` 1
          length cids `shouldBe` 1
