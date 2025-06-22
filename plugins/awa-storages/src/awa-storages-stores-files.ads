-----------------------------------------------------------------------
--  awa-storages-stores-files -- File system store
--  Copyright (C) 2012, 2016, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with ADO.Sessions;

with ASF.Applications.Main.Configs;
with AWA.Storages.Models;

--  === File System store ===
--  The `AWA.Storages.Stores.Files` store uses the file system to save a data content.
--  Files are stored in a directory tree whose path is created from the workspace identifier
--  and the storage identifier.  The layout is such that files belonged to a given workspace
--  are stored in the same directory sub-tree.
--
--  The root directory of the file system store is configured through the
--  <b>storage_root</b> and <b>tmp_storage_root</b> configuration properties.
package AWA.Storages.Stores.Files is

   --  Parameter that indicates the root directory for the file storage.
   package Root_Directory_Parameter is
     new ASF.Applications.Main.Configs.Parameter (Name    => "storage_root",
                                                  Default => "storage");

   --  Parameter that indicates the root directory for a temporary file storage.
   package Tmp_Directory_Parameter is
     new ASF.Applications.Main.Configs.Parameter (Name    => "tmp_storage_root",
                                                  Default => "tmp");

   --  ------------------------------
   --  Storage Service
   --  ------------------------------
   type File_Store (Len : Natural) is new AWA.Storages.Stores.Store with private;
   type File_Store_Access is access all File_Store'Class;

   --  Create a file storage service and use the <tt>Root</tt> directory to store the files.
   function Create_File_Store (Root : in String) return Store_Access;

   --  Save the file represented by the `Path` variable into a store and associate that
   --  content with the storage reference represented by `Into`.
   overriding
   procedure Save (Storage : in File_Store;
                   Session : in out ADO.Sessions.Master_Session;
                   Into    : in out AWA.Storages.Models.Storage_Ref'Class;
                   Path    : in String);

   --  Load the storage item represented by `From` in a file that can be accessed locally.
   overriding
   procedure Load (Storage : in File_Store;
                   Session : in out ADO.Sessions.Session'Class;
                   From    : in AWA.Storages.Models.Storage_Ref'Class;
                   Into    : in out AWA.Storages.Storage_File);

   --  Create a storage
   overriding
   procedure Create (Storage : in File_Store;
                     Session : in out ADO.Sessions.Master_Session;
                     From    : in AWA.Storages.Models.Storage_Ref'Class;
                     Into    : in out AWA.Storages.Storage_File);

   --  Delete the content associate with the storage represented by `From`.
   overriding
   procedure Delete (Storage : in File_Store;
                     Session : in out ADO.Sessions.Master_Session;
                     From    : in out AWA.Storages.Models.Storage_Ref'Class);

   --  Build a path where the file store represented by <tt>Store</tt> is saved.
   function Get_Path (Storage : in File_Store;
                      Store   : in AWA.Storages.Models.Storage_Ref'Class) return String;

   --  Build a path where the file store represented by <tt>Store</tt> is saved.
   function Get_Path (Storage      : in File_Store;
                      Workspace_Id : in ADO.Identifier;
                      File_Id      : in ADO.Identifier) return String;

private

   type File_Store (Len : Natural) is new AWA.Storages.Stores.Store with record

      --  The root directory that contains the file system storage.
      Root : String (1 .. Len);
   end record;

end AWA.Storages.Stores.Files;
