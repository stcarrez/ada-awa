-----------------------------------------------------------------------
--  awa-storages -- Storage module
--  Copyright (C) 2012, 2016 Stephane Carrez
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
with Ada.Directories;
package body AWA.Storages is

   --  ------------------------------
   --  Get the path to get access to the file.
   --  ------------------------------
   function Get_Path (File : in Storage_File) return String is
   begin
      return Ada.Strings.Unbounded.To_String (File.Path);
   end Get_Path;

   --  ------------------------------
   --  Set the file path for the FILE, URL, CACHE or TMP storage.
   --  ------------------------------
   procedure Set (File : in out Storage_File;
                  Path : in String) is
   begin
      File.Path := Ada.Strings.Unbounded.To_Unbounded_String (Path);
   end Set;

   --  ------------------------------
   --  Set the file database storage identifier.
   --  ------------------------------
   procedure Set (File      : in out Storage_File;
                  Workspace : in ADO.Identifier;
                  Store     : in ADO.Identifier) is
   begin
      File.Workspace := Workspace;
      File.Store     := Store;
   end Set;

   overriding
   procedure Finalize (File : in out Storage_File) is
   begin
      if File.Storage = TMP and then Ada.Strings.Unbounded.Length (File.Path) > 0 then
         declare
            Path : constant String := Ada.Strings.Unbounded.To_String (File.Path);
         begin
            if Ada.Directories.Exists (Path) then
               Ada.Directories.Delete_File (Path);
            end if;
         end;
      end if;
   end Finalize;

end AWA.Storages;
