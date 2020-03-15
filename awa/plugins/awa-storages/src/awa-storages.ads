-----------------------------------------------------------------------
--  awa-storages -- Storage module
--  Copyright (C) 2012, 2015, 2016, 2018, 2020 Stephane Carrez
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
with Ada.Strings.Unbounded;
with Ada.Finalization;

with ADO;

--  = Storages Module =
--  The `storages` module provides a set of storage services allowing
--  an application to store data files, documents, images in a persistent area.
--  The persistent store can be on a file system, in the database or provided
--  by a remote service such as Amazon Simple Storage Service.
--
--  @include awa-storages-modules.ads
--
--  == Creating a storage ==
--  A data in the storage is represented by a `Storage_Ref` instance.
--  The data itself can be physically stored in a file system (`FILE` mode),
--  in the database (`DATABASE` mode) or on a remote server (`URL` mode).
--  To put a file in the storage space, first create the storage object
--  instance:
--
--    Data : AWA.Storages.Models.Storage_Ref;
--
--  Then setup the storage mode that you want.  The storage service uses
--  this information to save the data in a file, in the database or in
--  a remote service (in the future).
--  To save a file in the store, we can use the `Save` operation of the
--  storage service.
--  It will read the file and put in in the corresponding persistent store
--  (the database in this example).
--
--    Service.Save (Into => Data, Path => Path_To_The_File,
--                  Storage => AWA.Storages.Models.DATABASE);
--
--  Upon successful completion, the storage instance `Data` will be allocated
--  a unique identifier that can be retrieved by `Get_Id` or `Get_Key`.
--
--  == Getting the data ==
--  Several operations are defined to retrieve the data.  Each of them has been
--  designed to optimize the retrieval and
--
--    * The data can be retrieved in a local file.
--      This mode is useful if an external program must be launched and be able
--      to read the file.  If the storage mode of the data is `FILE`, the path
--      of the file on the storage file system is used.  For other storage modes,
--      the file is saved in a temporary file.  In that case the `Store_Local`
--      database table is used to track such locally saved data.
--
--    * The data can be returned as a stream.
--      When the application has to read the data, opening a read stream
--      connection is the most efficient mechanism.
--
--  == Local file ==
--  To access the data by using a local file, we must define a local storage
--  reference:
--
--    Data : AWA.Storages.Models.Store_Local_Ref;
--
--  and use the `Load` operation with the storage identifier.  When loading
--  locally we also indicate whether the file will be read or written.  A file
--  that is in `READ` mode can be shared by several tasks or processes.
--  A file that is in `WRITE` mode will have a specific copy for the caller.
--  An optional expiration parameter indicate when the local file representation
--  can expire.
--
--    Service.Load (From => Id, Into => Data, Mode => READ, Expire => ONE_DAY);
--
--  Once the load operation succeeded, the data is stored on the file system and
--  the local path is obtained by using the `Get_Path` operation:
--
--    Path : constant String := Data.Get_Path;
--
--  @include awa-storages-services.ads
--
--  == Ada Beans ==
--  @include-bean storages.xml
--  @include-bean storage-list.xml
--  @include-bean folder-queries.xml
--  @include-bean storage-queries.xml
--
--  @include awa-storages-servlets.ads
--
--  == Queries ==
--  @include-query storage-list.xml
--  @include-query folder-queries.xml
--  @include-query storage-queries.xml
--
--  == Data model ==
--  [images/awa_storages_model.png]
--
package AWA.Storages is

   type Storage_Type is (DATABASE, FILE, URL, CACHE, TMP);

   type Storage_File (Storage : Storage_Type) is tagged limited private;

   --  Get the path to get access to the file.
   function Get_Path (File : in Storage_File) return String;

   --  Set the file path for the FILE, URL, CACHE or TMP storage.
   procedure Set (File : in out Storage_File;
                  Path : in String);

   --  Set the file database storage identifier.
   procedure Set (File      : in out Storage_File;
                  Workspace : in ADO.Identifier;
                  Store     : in ADO.Identifier);

private

   type Storage_File (Storage : Storage_Type) is limited
   new Ada.Finalization.Limited_Controlled with record
      case Storage is
         when DATABASE =>
            Workspace : ADO.Identifier;
            Store     : ADO.Identifier;

         when FILE | URL | CACHE | TMP =>
            Path    : Ada.Strings.Unbounded.Unbounded_String;

      end case;
   end record;

   overriding
   procedure Finalize (File : in out Storage_File);

end AWA.Storages;
