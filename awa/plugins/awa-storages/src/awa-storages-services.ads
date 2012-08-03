-----------------------------------------------------------------------
--  awa-storages-services -- Storage service
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
with Ada.Calendar;
with Ada.Strings.Unbounded;

with Security.Permissions;

with ADO;
with ASF.Parts;
with AWA.Modules;

with AWA.Storages.Models;
with AWA.Storages.Stores;
with AWA.Storages.Stores.Databases;
package AWA.Storages.Services is

   package ACL_Create_Storage is new Security.Permissions.Permission_ACL ("storage-create");
   package ACL_Delete_Storage is new Security.Permissions.Permission_ACL ("storage-delete");
   package ACL_Create_Folder is new Security.Permissions.Permission_ACL ("folder-create");

   type Read_Mode is (READ, WRITE);

   type Expire_Type is (ONE_HOUR, ONE_DAY, TWO_DAYS, ONE_WEEK, ONE_YEAR, NEVER);

   --  ------------------------------
   --  Storage Service
   --  ------------------------------
   --  The <b>Storage_Service</b> defines a set of operations to store and retrieve
   --  a data object from the persistent storage.  The data object is treated as a raw
   --  byte stream.  The persistent storage can be implemented by a database, a file
   --  system or a remote service such as Amazon AWS.
   type Storage_Service is new AWA.Modules.Module_Manager with private;
   type Storage_Service_Access is access all Storage_Service'Class;

   --  Initializes the storage service.
   overriding
   procedure Initialize (Service : in out Storage_Service;
                         Module  : in AWA.Modules.Module'Class);

   --  Get the persistent store that manages the data represented by <tt>Data</tt>.
   function Get_Store (Service : in Storage_Service;
                       Data    : in AWA.Storages.Models.Storage_Ref'Class)
                       return AWA.Storages.Stores.Store_Access;

   --  Create or save the folder.
   procedure Save_Folder (Service : in Storage_Service;
                          Folder  : in out AWA.Storages.Models.Storage_Folder_Ref'Class);

   --  Load the folder identified by the given id.
   procedure Load_Folder (Service : in Storage_Service;
                          Folder  : in out AWA.Storages.Models.Storage_Folder_Ref'Class;
                          Id      : in ADO.Identifier);

   --  Save the data object contained in the <b>Data</b> part element into the
   --  target storage represented by <b>Into</b>.
   procedure Save (Service : in Storage_Service;
                   Into    : in out AWA.Storages.Models.Storage_Ref'Class;
                   Data    : in ASF.Parts.Part'Class;
                   Storage : in AWA.Storages.Models.Storage_Type);

   --  Save the file pointed to by the <b>Path</b> string in the storage
   --  object represented by <b>Into</b> and managed by the storage service.
   procedure Save (Service : in Storage_Service;
                   Into    : in out AWA.Storages.Models.Storage_Ref'Class;
                   Path    : in String;
                   Storage : in AWA.Storages.Models.Storage_Type);

   --  Load the storage content identified by <b>From</b> in a local file
   --  that will be identified by <b>Into</b>.
   procedure Load (Service : in Storage_Service;
                   From    : in AWA.Storages.Models.Storage_Ref'Class;
                   Into    : in out AWA.Storages.Models.Store_Local_Ref'Class);

   --  Load the storage content identified by <b>From</b> into the blob descriptor <b>Into</b>.
   --  Raises the <b>NOT_FOUND</b> exception if there is no such storage.
   procedure Load (Service : in Storage_Service;
                   From    : in ADO.Identifier;
                   Mime    : out Ada.Strings.Unbounded.Unbounded_String;
                   Date    : out Ada.Calendar.Time;
                   Into    : out ADO.Blob_Ref);

   --  Load the storage content into a file.  If the data is not stored in a file, a temporary
   --  file is created with the data content fetched from the store (ex: the database).
   --  The `Mode` parameter indicates whether the file will be read or written.
   --  The `Expire` parameter allows to control the expiration of the temporary file.
   procedure Load (Service : in Storage_Service;
                   From    : in ADO.Identifier;
                   Into    : out AWA.Storages.Models.Store_Local_Ref;
                   Mode    : in Read_Mode := READ;
                   Expire  : in Expire_Type := ONE_DAY);

   --  Deletes the storage instance.
   procedure Delete (Service : in Storage_Service;
                     Storage : in out AWA.Storages.Models.Storage_Ref'Class);

   --  Deletes the storage instance.
   procedure Delete (Service : in Storage_Service;
                     Storage : in ADO.Identifier);

private

   type Store_Access_Array is
     array (AWA.Storages.Models.Storage_Type) of AWA.Storages.Stores.Store_Access;

   type Storage_Service is new AWA.Modules.Module_Manager with record
      Stores         : Store_Access_Array;
      Database_Store : aliased AWA.Storages.Stores.Databases.Database_Store;
   end record;

end AWA.Storages.Services;
