-----------------------------------------------------------------------
--  awa-storages-stores-databases -- Database store
--  Copyright (C) 2012, 2020 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
-----------------------------------------------------------------------
with ADO.Sessions;

with ASF.Applications.Main.Configs;
with AWA.Storages.Models;

--  === Database store ===
--  The `AWA.Storages.Stores.Databases` store uses the database to save a data content.
--  The data is saved in a specific table in a database blob column.
--  The database store uses another store service to temporarily save the data content
--  in a local file when the application needs a file access to the data.
package AWA.Storages.Stores.Databases is

   --  Parameter that indicates the maximum size of files stored in the database.
   package Max_Size_Parameter is
     new ASF.Applications.Main.Configs.Parameter (Name    => "database_max_size",
                                                  Default => "100000");

   --  ------------------------------
   --  Storage Service
   --  ------------------------------
   type Database_Store is new AWA.Storages.Stores.Store with record
      Tmp : AWA.Storages.Stores.Store_Access;
   end record;

   --  Save the file represented by the `Path` variable into a store and associate that
   --  content with the storage reference represented by `Into`.
   procedure Save (Storage : in Database_Store;
                   Session : in out ADO.Sessions.Master_Session;
                   Into    : in out AWA.Storages.Models.Storage_Ref'Class;
                   Path    : in String);

   procedure Load (Storage : in Database_Store;
                   Session : in out ADO.Sessions.Session'Class;
                   From    : in AWA.Storages.Models.Storage_Ref'Class;
                   Into    : in out AWA.Storages.Storage_File);

   --  Create a storage
   procedure Create (Storage : in Database_Store;
                     Session : in out ADO.Sessions.Master_Session;
                     From    : in AWA.Storages.Models.Storage_Ref'Class;
                     Into    : in out AWA.Storages.Storage_File);

   --  Delete the content associate with the storage represented by `From`.
   procedure Delete (Storage : in Database_Store;
                     Session : in out ADO.Sessions.Master_Session;
                     From    : in out AWA.Storages.Models.Storage_Ref'Class);

end AWA.Storages.Stores.Databases;
