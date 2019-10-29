# Quokka

Is a small library that helps developers generate test data for the purpose
of writing test/specs against their code which read and write data to the
database.

![Quokka](quokka.jpg)

## Limitations

This library currently makes some technical assumptions listed below. Some of these may
be addressed in a future version.

* This library only works with Postgres version 9.1+. It uses the Haskell `postgresql-simple`
  library.
* It only works with tables that uses the `Integer` type for the primary key, and
  by virtue foreign keys which are based on the same type.
* It also relies on schemas which follow certain conventions;
  * Primary keys in your database need to be of type `Integer`, and also need to
     be named `Id`.
  * Foreign keys need to be named using the convention `tableName_id` where tableName
     is the singular form of the table name. So for example if you have a `users` table
     and an `accounts` table, and the `users` table has a foreign key to the accounts
     table, then the foreign key in the `accounts` table needs to be named `user_id`.

## Getting Started

To get started you will need to run the script `./bin/db-refresh`, this script
requires an up and running Postgres server which can be installed by following the
steps below;

### Postgres Install / Configure

#### OS-X

```
## install postgresql
brew install postgresql

## to initialize (once only)
initdb /usr/local/var/postgres -E utf8

## to start
pg_ctl -D /usr/local/var/postgres -l /usr/local/var/postgres/server.log start

## to stop
pg_ctl -D /usr/local/var/postgres stop -s -m fast
```

Setting up DB for tests.

```
./bin/db-refresh
```

#### Linux (Debian)

```
## install postgresql
apt-get install postgresql postgresql-client libpq-dev

# For the bin/db* scripts to work you have to setup your user to have superuser permissions
# Something like:

# Login as the default database user postgres
sudo -u postgres bash

# Start the client
psql

# Create a user with your username and set permissions
create user <your-username>;
alter role <your-username> with superuser;
alter user <your-username> with encrypted password '<password>';
```

## Breakdown

If you are the type of person that feels reading documentation is a waste of time,
then I highly recommend you look in the test folder of this project to learn
how to use this particular library, otherwise please read on.

To get started with this library you need to define your `ParentTable` and `ChildTable`
tables. The `ParentTable` type represents a table in your relational database that
has no foreign keys, and has a primary key column named `Id` of type `Integer`.
The `ChildTable` type is table in your relational database that has 1 or more
foreign keys, and has  a primary key column named `Id` of type `Integer`. As mentioned
in the [Limitations](##Limitations) section foreign keys currently need to be named using
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
accountIds <- insertAccounts conn [("Johns Account" :: Text, "Description" :: Text, id' userIds)]
```

The function `id'` is a helper function available in the library to extract the
foreign key value of the parent entity when generating the `insert` statement
for the database.

### Populating Associate Tables

Associate tables are a special case of child table with 2 foreign key associations.
Populating associate tables requires the resolution of 2 foreign keys. Similar
to child tables an associate table can be modelled using two `ParentTable` types
and a single `ChildTable` type. This is illustrated in the example below which
builds on the example from the [Populating Parent Tables](###Populating-Parent-Tables)
section;

```Haskell
insertProfiles :: (ToRow q) => Connection -> [q] -> IO [Id]
insertProfiles conn = buildWithManyRels conn [userTable, accountTableAsParent] profileTable

userIds <- insertUsers conn [("John" :: Text, 40 :: Int, False :: Bool)]
accountIds <- insertAccounts conn [("Johns Account" :: Text, "Description" :: Text, id' userIds)]
profileIds <- insertProfiles conn [(True :: Bool, id' userIds, id' accountIds)]
```

`buildWithManyRels` in this case returns a `Query` type which is aware of the
associative nature of `profiles` with `users` and `accounts`. So when we invoke
the `insertProfiles` function the foreign key values are resolved during the
insert. `Quokka` uses concrete types in a very restrictive way. So the definition
of the `Account` child table cannot be used in the function call `buildWithManyRels`
so we had to define the `accounts` table twice, once as `ParentTable` type, and
once as a `ChildTable` type.

### Insert Single Record

In the examples presented we see that even though we insert a single row the API
treats everything as a collection. This can be a bit tiresome when we only want
to insert one tuple. For this we have a whole series of functions that deal with
a single record. For example to insert 1 single parent tuple with no relations we
use the function `build1` instead of `build`. Similarly to build a parent and
child relationship with a single tuple you can use the `build1With1Rel` instead
off the `buildWith1Rel` function so on and so forth.

## License

MIT License

Copyright (c) 2019 Shirren Premaratne

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
