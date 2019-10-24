{-# LANGUAGE OverloadedStrings #-}

module Quokka.Tables (
  accountTableAsChild
, id'
, insertAccounts
, insertProfiles
, insertUsers
, userTable
) where

import Database.PostgreSQL.Simple (Connection, ToRow)
import Quokka.Types (ChildTable (..), Id (..), ParentTable (..))
import Quokka.Functions (build, buildWith1Rel, buildWithManyRels)


userTable :: ParentTable
userTable =
  ParentTable "users" ["firstname", "lastname", "age"]


accountTableAsParent :: ParentTable
accountTableAsParent =
  ParentTable "accounts" ["name", "description"]


accountTableAsChild :: ChildTable
accountTableAsChild =
  ChildTable "accounts" ["name", "description"]


profileTable :: ChildTable
profileTable =
  ChildTable "profiles" ["active"]


insertUsers
  :: (ToRow q)
  => Connection
  -> [q]
  -> IO [Id]
insertUsers conn =
  build conn userTable


insertAccounts
  :: (ToRow q)
  => Connection
  -> [q]
  -> IO [Id]
insertAccounts conn =
  buildWith1Rel conn userTable accountTableAsChild


insertProfiles
  :: (ToRow q)
  => Connection
  -> [q]
  -> IO [Id]
insertProfiles conn =
  buildWithManyRels conn [userTable, accountTableAsParent] profileTable


id' :: [Id] -> Int
id' pids =
  getId (head pids)