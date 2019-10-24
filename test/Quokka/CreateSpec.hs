{-# LANGUAGE OverloadedStrings #-}

module Quokka.CreateSpec (
  spec
) where

import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Database.PostgreSQL.Simple.Types (Query (Query))
import Quokka.Types (ChildTable (..), ParentTable (..))
import Quokka.Functions (insertStatement
                        , insertStatementWith1Rel
                        , insertStatementWithManyRels)
import Quokka.Helper (setupDb, withDatabaseConnection)
import Quokka.Tables (id', insertAccounts, insertProfiles, insertUsers)
import Test.Hspec


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


  describe "insertStatementWithManyRels" $ do
    context "for 2 parent tables" $
      it "should return an insert statement with 2 FKs set" $ do
        let
          parentTable1 = ParentTable "users" ["firstname", "lastname", "age"]
          parentTable2 = ParentTable "accounts" ["name"]
          childTable   = ChildTable "profiles" ["active"]
          Query stmt   = insertStatementWithManyRels [parentTable1, parentTable2] childTable

        stmt `shouldBe` encodeUtf8 "insert into profiles (active,user_id,account_id) values (?,?,?) returning id;"


    context "for no parents" $
      it "should return an insert statement with no FKs set" $ do
        let
          childTable   = ChildTable "profiles" ["active"]
          Query stmt  = insertStatementWithManyRels [] childTable

        stmt `shouldBe` encodeUtf8 "insert into profiles (active) values (?) returning id;"


  before_ setupDb $
    around withDatabaseConnection $
      describe "insert" $ do
        context "for a table with no relationships" $
          it "should insert data into the database" $ \conn -> do
            userIds <- insertUsers conn [
                ("John" :: Text, "Doe" :: Text, Just 1 :: Maybe Int)
              , ("Jane" :: Text, "Doe" :: Text, Nothing)]

            length userIds `shouldBe` 2


        context "for a table with a single relationship" $
          it "should insert parent and child into the database" $ \conn -> do
            userIds    <- insertUsers conn [("John" :: Text, "Doe" :: Text, 1 :: Int)]
            accountIds <- insertAccounts conn [("Account-1" :: Text, "Description" :: Text, id' userIds)]

            length userIds `shouldBe` 1
            length accountIds `shouldBe` 1


        context "for a table with multiple relationships" $
          it "should insert parent and children into the database" $ \conn -> do
            userIds    <- insertUsers conn [("John" :: Text, "Doe" :: Text, 1 :: Int)]
            accountIds <- insertAccounts conn [("Account-1" :: Text, "Description" :: Text, id' userIds)]
            profileIds <- insertProfiles conn [(True :: Bool, id' userIds, id' accountIds)]

            length userIds `shouldBe` 1
            length accountIds `shouldBe` 1
            length profileIds `shouldBe` 1
