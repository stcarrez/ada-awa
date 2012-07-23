-----------------------------------------------------------------------
--  awa-storages-stores-files -- File system store
--  Copyright (C) 2012 Stephane Carrez
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

with AWA.Storages.Models;

--  === File Syste; store ===
--  The `AWA.Storages.Stores.Files` store uses the file system to save a data content.
--
package AWA.Storages.Stores.Files is

   --  ------------------------------
   --  Storage Service
   --  ------------------------------
   type File_Store (Len : Natural) is new AWA.Storages.Stores.Store with private;

   --  Save the file represented by the `Path` variable into a store and associate that
   --  content with the storage reference represented by `Into`.
   procedure Save (Storage : in File_Store;
                   Session : in out ADO.Sessions.Master_Session;
                   Into    : in out AWA.Storages.Models.Storage_Ref'Class;
                   Path    : in String);

   procedure Load (Storage : in File_Store;
                   Session : in out ADO.Sessions.Master_Session;
                   From    : in AWA.Storages.Models.Storage_Ref'Class;
                   Into    : in String);

   --  Delete the content associate with the storage represented by `From`.
   procedure Delete (Storage : in File_Store;
                     Session : in out ADO.Sessions.Master_Session;
                     From    : in out AWA.Storages.Models.Storage_Ref'Class);

   --  Build a path where the file store represented by <tt>Store</tt> is saved.
   function Get_Path (Storage : in File_Store;
                      Store   : in AWA.Storages.Models.Storage_Ref'Class) return String;

private

   type File_Store (Len : Natural) is new AWA.Storages.Stores.Store with record

      --  The root directory that contains the file system storage.
      Root : String (1 .. Len);
   end record;

end AWA.Storages.Stores.Files;
