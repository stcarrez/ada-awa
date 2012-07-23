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
with Ada.Streams;
with Ada.Directories;
with Interfaces;

with Util.Files;
with Util.Encoders;
with Util.Encoders.Base64;
package body AWA.Storages.Stores.Files is

   --  ------------------------------
   --  Build a path where the file store represented by <tt>Store</tt> is saved.
   --  ------------------------------
   function Get_Path (Storage : in File_Store;
                      Store   : in AWA.Storages.Models.Storage_Ref'Class) return String is
      use Interfaces;
      use type Ada.Streams.Stream_Element_Offset;

      T      : Util.Encoders.Base64.Encoder;
      Buffer : Ada.Streams.Stream_Element_Array (1 .. 10);
      R      : Ada.Streams.Stream_Element_Array (1 .. 32);
      Last   : Ada.Streams.Stream_Element_Offset;
      Encoded : Ada.Streams.Stream_Element_Offset;
      Pos  : Positive := 1;
      Res : String (1 .. 16 + 5);
      Workspace_Id : constant ADO.Identifier := Store.Get_Workspace.Get_Id;
   begin
      Util.Encoders.Encode_LEB128 (Buffer, Buffer'First, Unsigned_64 (Workspace_Id), Last);
      Util.Encoders.Encode_LEB128 (Buffer, Last, Unsigned_64 (Store.Get_Id), Last);

      T.Transform (Data    => Buffer (1 .. Last),
                   Into    => R, Last => Last,
                   Encoded => Encoded);

      for I in 1 .. Last loop
         Res (Pos) := Character'Val (R (I));
         Pos := Pos + 1;
         if (I mod 2) = 0 and I /= Last then
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
   procedure Save (Storage : in File_Store;
                   Session : in out ADO.Sessions.Master_Session;
                   Into    : in out AWA.Storages.Models.Storage_Ref'Class;
                   Path    : in String) is
      pragma Unreferenced (Session);

      Store : constant String := Storage.Get_Path (Into);
      Dir   : constant String := Ada.Directories.Containing_Directory (Store);
   begin
      Ada.Directories.Create_Path (Dir);
      Ada.Directories.Copy_File (Source_Name => Path,
                                 Target_Name => Store,
                                 Form        => "all");
   end Save;

   procedure Load (Storage : in File_Store;
                   Session : in out ADO.Sessions.Master_Session;
                   From    : in AWA.Storages.Models.Storage_Ref'Class;
                   Into    : in String) is
   begin
      null;
   end Load;

   --  ------------------------------
   --  Delete the content associate with the storage represented by `From`.
   --  ------------------------------
   procedure Delete (Storage : in File_Store;
                     Session : in out ADO.Sessions.Master_Session;
                     From    : in out AWA.Storages.Models.Storage_Ref'Class) is
      pragma Unreferenced (Session);

      Store : constant String := Storage.Get_Path (From);
   begin
      if Ada.Directories.Exists (Store) then
         Ada.Directories.Delete_File (Store);
      end if;
   end Delete;

end AWA.Storages.Stores.Files;
