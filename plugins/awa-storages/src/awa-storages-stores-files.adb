-----------------------------------------------------------------------
--  awa-storages-stores-files -- File system store
--  Copyright (C) 2012, 2015, 2016, 2018, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Streams;
with Ada.Directories;
with Interfaces;

with Util.Files;
with Util.Log.Loggers;
with Util.Encoders;
with Util.Encoders.Base64;
package body AWA.Storages.Stores.Files is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("AWA.Storages.Stores.Files");

   --  ------------------------------
   --  Create a file storage service and use the <tt>Root</tt> directory to store the files.
   --  ------------------------------
   function Create_File_Store (Root : in String) return Store_Access is
      Result : constant File_Store_Access := new File_Store '(Len => Root'Length,
                                                              Root => Root);
   begin
      return Result.all'Access;
   end Create_File_Store;

   --  ------------------------------
   --  Build a path where the file store represented by <tt>Store</tt> is saved.
   --  ------------------------------
   function Get_Path (Storage : in File_Store;
                      Store   : in AWA.Storages.Models.Storage_Ref'Class) return String is
   begin
      return Storage.Get_Path (Store.Get_Workspace.Get_Id, Store.Get_Id);
   end Get_Path;

   --  ------------------------------
   --  Build a path where the file store represented by <tt>Store</tt> is saved.
   --  ------------------------------
   function Get_Path (Storage      : in File_Store;
                      Workspace_Id : in ADO.Identifier;
                      File_Id      : in ADO.Identifier) return String is
      use Interfaces;
      use type Ada.Streams.Stream_Element_Offset;

      T      : Util.Encoders.Base64.Encoder;
      Buffer : Ada.Streams.Stream_Element_Array (1 .. 10);
      R      : Ada.Streams.Stream_Element_Array (1 .. 32);
      Last   : Ada.Streams.Stream_Element_Offset;
      Encoded : Ada.Streams.Stream_Element_Offset;
      Pos  : Positive := 1;
      Res : String (1 .. 16 + 5);
   begin
      Util.Encoders.Encode_LEB128 (Buffer, Buffer'First, Unsigned_64 (Workspace_Id), Last);
      Util.Encoders.Encode_LEB128 (Buffer, Last, Unsigned_64 (File_Id), Last);

      T.Transform (Data    => Buffer (1 .. Last),
                   Into    => R, Last => Last,
                   Encoded => Encoded);

      for I in 1 .. Last loop
         Res (Pos) := Character'Val (R (I));
         Pos := Pos + 1;
         if (I mod 2) = 0 and then I /= Last then
            Res (Pos) := '/';
            Pos := Pos + 1;
         end if;
      end loop;
      return Util.Files.Compose (Storage.Root, Res (1 .. Pos - 1));
   end Get_Path;

   --  ------------------------------
   --  Save the file represented by the `Path` variable into a store and associate that
   --  content with the storage reference represented by `Into`.
   --  ------------------------------
   overriding
   procedure Save (Storage : in File_Store;
                   Session : in out ADO.Sessions.Master_Session;
                   Into    : in out AWA.Storages.Models.Storage_Ref'Class;
                   Path    : in String) is
      pragma Unreferenced (Session);

      Store : constant String := Storage.Get_Path (Into);
      Dir   : constant String := Ada.Directories.Containing_Directory (Store);
   begin
      Log.Info ("Storage save {0} to {1}", Path, Store);
      Ada.Directories.Create_Path (Dir);
      Ada.Directories.Copy_File (Source_Name => Path,
                                 Target_Name => Store,
                                 Form        => "all");
   end Save;

   overriding
   procedure Load (Storage : in File_Store;
                   Session : in out ADO.Sessions.Session'Class;
                   From    : in AWA.Storages.Models.Storage_Ref'Class;
                   Into    : in out Storage_File) is
      pragma Unreferenced (Session);

      Store : constant String := Storage.Get_Path (From);
   begin
      Into.Path := Ada.Strings.Unbounded.To_Unbounded_String (Store);
   end Load;

   --  ------------------------------
   --  Create a storage
   --  ------------------------------
   overriding
   procedure Create (Storage : in File_Store;
                     Session : in out ADO.Sessions.Master_Session;
                     From    : in AWA.Storages.Models.Storage_Ref'Class;
                     Into    : in out AWA.Storages.Storage_File) is
      pragma Unreferenced (Session);

      Store : constant String := Storage.Get_Path (From);
      Dir   : constant String := Ada.Directories.Containing_Directory (Store);
   begin
      Log.Info ("Storage create {0}", Store);
      Ada.Directories.Create_Path (Dir);
      Into.Path := Ada.Strings.Unbounded.To_Unbounded_String (Store);
   end Create;

   --  ------------------------------
   --  Delete the content associate with the storage represented by `From`.
   --  ------------------------------
   overriding
   procedure Delete (Storage : in File_Store;
                     Session : in out ADO.Sessions.Master_Session;
                     From    : in out AWA.Storages.Models.Storage_Ref'Class) is
      pragma Unreferenced (Session);

      Store : constant String := Storage.Get_Path (From);
   begin
      if Ada.Directories.Exists (Store) then
         Log.Info ("Storage delete {0}", Store);
         Ada.Directories.Delete_File (Store);
      end if;
   end Delete;

end AWA.Storages.Stores.Files;
