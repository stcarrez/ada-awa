-----------------------------------------------------------------------
--  awa-storages-stores-databases -- Database store
--  Copyright (C) 2012, 2015, 2016, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Streams.Stream_IO;
with Util.Log.Loggers;
package body AWA.Storages.Stores.Databases is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("AWA.Storages.Stores.Files");

   --  Create a storage
   overriding
   procedure Create (Storage : in Database_Store;
                     Session : in out ADO.Sessions.Master_Session;
                     From    : in AWA.Storages.Models.Storage_Ref'Class;
                     Into    : in out AWA.Storages.Storage_File) is
   begin
      Storage.Tmp.Create (Session, From, Into);
   end Create;

   --  ------------------------------
   --  Save the file represented by the `Path` variable into a store and associate that
   --  content with the storage reference represented by `Into`.
   --  ------------------------------
   overriding
   procedure Save (Storage : in Database_Store;
                   Session : in out ADO.Sessions.Master_Session;
                   Into    : in out AWA.Storages.Models.Storage_Ref'Class;
                   Path    : in String) is
      pragma Unreferenced (Storage);

      Store : AWA.Storages.Models.Storage_Data_Ref;
      Blob  : constant ADO.Blob_Ref := ADO.Create_Blob (Path);
   begin
      Log.Info ("Save database file {0}", Path);
      Store.Set_Data (Blob);
      Store.Save (Session);
      Into.Set_File_Size (Natural (Blob.Value.Len));
      Into.Set_Store_Data (Store);
   end Save;

   overriding
   procedure Load (Storage : in Database_Store;
                   Session : in out ADO.Sessions.Session'Class;
                   From    : in AWA.Storages.Models.Storage_Ref'Class;
                   Into    : in out AWA.Storages.Storage_File) is
      Store : AWA.Storages.Models.Storage_Data_Ref'Class := From.Get_Store_Data;
      File  : Ada.Streams.Stream_IO.File_Type;
      DB    : ADO.Sessions.Master_Session := ADO.Sessions.Master_Session (Session);
   begin
      Storage.Tmp.Create (DB, From, Into);
      Log.Info ("Load database file {0} to {1}",
                ADO.Identifier'Image (Store.Get_Id), Get_Path (Into));
      Store.Load (Session, Store.Get_Id);
      Ada.Streams.Stream_IO.Create (File => File,
                                    Mode => Ada.Streams.Stream_IO.Out_File,
                                    Name => Get_Path (Into));
      Ada.Streams.Stream_IO.Write (File, Store.Get_Data.Value.Data);
      Ada.Streams.Stream_IO.Close (File);
   end Load;

   --  ------------------------------
   --  Delete the content associate with the storage represented by `From`.
   --  ------------------------------
   overriding
   procedure Delete (Storage : in Database_Store;
                     Session : in out ADO.Sessions.Master_Session;
                     From    : in out AWA.Storages.Models.Storage_Ref'Class) is
      pragma Unreferenced (Storage);

      Store : AWA.Storages.Models.Storage_Data_Ref'Class := From.Get_Store_Data;
   begin
      if not Store.Is_Null then
         Log.Info ("Delete file {0}", ADO.Identifier'Image (From.Get_Id));
         Store.Delete (Session);
      end if;
   end Delete;

end AWA.Storages.Stores.Databases;
