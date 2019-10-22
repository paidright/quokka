{-# LANGUAGE OverloadedStrings #-}

module Quokka.CreateSpec (
  spec
) where

import Data.Text (Text)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types (Query (Query))
import Quokka.Types (Table(..), Id (..))
import Quokka.Functions (createStatement, create)
import Test.Hspec


localConnection :: IO Connection
localConnection =
  connect defaultConnectInfo { connectPassword = "", connectDatabase = "quokka_dev" }


spec :: Spec
spec = do
  describe "createStatement" $ do
    it "should return an insert statement for a single column" $ do
      let 
        table = Table "users" ["name"] 
        stmt  = createStatement table
      stmt `shouldBe` Query "insert into users (name) values (?) returning id;"

    it "should return an insert statement for multiple columns" $ do
      let 
        table = Table "users" ["firstname", "lastname"] 
        stmt  = createStatement table
      stmt `shouldBe` Query "insert into users (firstname,lastname) values (?,?) returning id;"

  describe "insert" $ do
    it "should insert data into the database" $ do
      conn <- localConnection
      let 
        table = Table "users" ["firstname", "lastname", "age"]
        insert = create conn table
      res  <- insert [("John" :: Text, "Doe" :: Text, Just 1 :: Maybe Int)
                     , ("Jane" :: Text, "Doe" :: Text, Nothing)
                     ]
      (length res) `shouldBe` 2

    it "should insert parent and child into the database" $ do
      conn <- localConnection
      let
        parentTable = Table "users" ["firstname", "lastname", "age"]
        childTable  = Table "accounts" ["name", "user_id"]
        insertPt    = create conn parentTable
        insertCt    = create conn childTable
      pids <- insertPt [("John" :: Text, "Doe" :: Text, 1 :: Int)]
      cids <- insertCt [("Account" :: Text, getId (pids !! 0) :: Int)]
      (length pids) `shouldBe` 1
      (length cids) `shouldBe` 1

