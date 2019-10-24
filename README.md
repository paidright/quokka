# Quokka

Is a small library that helps developers generate test data for the purpose
of writing test/specs against there code which read and write data to the
database.

## Limitations

This library is in a beta stage and is authored to work with a very specific
project I was working on, and as such has some technical dependencies listed below;

* This library only works with Postgres version 9.1+. It uses the Haskell `postgresql-simple`
  library.
* It only works with tables that uses the `Integer` type for the primary key, and
  by virtue foreign keys which are based on the same type.
* It also relies on schemas which follow certain conventions;
** Primary keys in your database need to be of type `Integer`, and also need to
   be named `Id`.
** Foreign keys need to be named using the convention `tableName_id` where tableName
   is the singular form of the table name. So for example if you have a `users` table
   and an `accounts` table, and the `users` table has a foreign key to the accounts
   table, then the foreign key in the `accounts` table needs to be named `user_id`.

## Getting Started

If you are the type of person that feels reading documentation is a waste of time,
then I highly recommend you look in the test folder of this project to learn
how to use this particular library, otherwise please read on.

To get started with this library you need to define your `ParentTable` and `ChildTable`
tables. The `ParentTable` type represents a table in your relational database that
has no foreign keys, and has a primary key column named `Id` of type `Integer`.
The `ChildTable` type is table in your relational database that has 1 or more
foreign keys, and has  a primary key column named `Id` of type `Integer`. As mentioned
in the [Limitations](Limitations) section foreign keys need to be named using
a particular convention.

Here are few examples of how to define your tables;

```Haskell
userTable :: ParentTable
userTable = ParentTable "users" ["name", "age", "active"]

accountTableAsParent :: ParentTable
accountTableAsParent = ParentTable "accounts" ["name", "description"]

accountTableAsChild :: ChildTable
accountTableAsChild = ChildTable "accounts" ["name", "description"]

profileTable :: ChildTable
profileTable = ChildTable "profiles" ["active"]
```

As you can see from the example above the `accounts` table has been defined
as both a parent table and child table. This is an example of how to represent
as associative table (Table which represents a many to many relationship)
using this library.

### Populating Parent Tables

Once your tables are defined then you can define your functions which can
take input data as a list of tuples which represent your data. Let's start
with the `users` table. The following example demonstrates how to define a
function to insert users, and then how to use this function;

```Haskell
insertUsers :: (ToRow q) => Connection -> [q] -> IO [Id]
insertUsers conn = build conn userTable

insertUsers conn [("John" :: Text, 40 :: Int, False :: Bool)
                 ,("Jane" :: Text, 32 :: Int, True :: Bool)]
```

The recommended pattern for setting this all up is to place such code in a
factory to create the data.

### Populating Child Tables

Populating child tables requires the resolution of foreign keys. `Quokka` is
aware of relationships through the `ChildTable` type. This type captures the
related table through the type thereby faciliating a mechanism through which
child rows can be inserted into the table. The following example demonstrates
how to define a function to insert child rows, and then how to use the
function. This example builds on the last example;

```Haskell
insertAccounts :: (ToRow q) => Connection -> [q] -> IO [Id]
insertAccounts conn = buildWith1Rel conn userTable accountTableAsChild

userIds <- insertUsers conn [("John" :: Text, 40 :: Int, False :: Bool)]
accoundIds <- insertAccounts conn [
  ("Johns Account" :: Text, "Description" :: Text, id' userIds)]
```
