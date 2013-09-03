-----------------------------------------------------------------------
--  awa-storages-beans -- Storage Ada Beans
--  Copyright (C) 2012, 2013 Stephane Carrez
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
with Ada.Containers;

with ADO;
with ADO.Utils;
with ADO.Queries;
with ADO.Sessions;
with ADO.Objects;
with ADO.Sessions.Entities;

with AWA.Workspaces.Models;
with AWA.Services.Contexts;
with AWA.Storages.Services;
package body AWA.Storages.Beans is

   --  ------------------------------
   --  Get the value identified by the name.
   --  ------------------------------
   overriding
   function Get_Value (From : in Upload_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = "folderId" then
         return ADO.Objects.To_Object (From.Get_Folder.Get_Key);
      elsif From.Is_Null then
         return Util.Beans.Objects.Null_Object;
      else
         return AWA.Storages.Models.Storage_Ref (From).Get_Value (Name);
      end if;
   end Get_Value;

   --  ------------------------------
   --  Set the value identified by the name.
   --  ------------------------------
   overriding
   procedure Set_Value (From  : in out Upload_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
      Manager : constant Services.Storage_Service_Access := From.Module.Get_Storage_Manager;
      Folder  : Models.Storage_Folder_Ref;
   begin
      if Name = "folderId" then
         Manager.Load_Folder (Folder, ADO.Utils.To_Identifier (Value));
         From.Set_Folder (Folder);
      elsif Name = "id" then
         Manager.Load_Storage (From, ADO.Utils.To_Identifier (Value));
      end if;
   end Set_Value;

   --  ------------------------------
   --  Save the uploaded file in the storage service.
   --  ------------------------------
   procedure Save_Part (Bean : in out Upload_Bean;
                        Part : in ASF.Parts.Part'Class) is
      Manager : constant Services.Storage_Service_Access := Bean.Module.Get_Storage_Manager;
   begin
      Bean.Set_Name (Part.Get_Name);
      Bean.Set_Mime_Type (Part.Get_Content_Type);
      Bean.Set_File_Size (Part.Get_Size);
      Manager.Save (Bean, Part, AWA.Storages.Models.DATABASE);
   end Save_Part;

   --  ------------------------------
   --  Upload the file.
   --  ------------------------------
   procedure Upload (Bean    : in out Upload_Bean;
                     Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      pragma Unreferenced (Bean);
   begin
      Outcome := Ada.Strings.Unbounded.To_Unbounded_String ("success");
   end Upload;

   --  ------------------------------
   --  Delete the file.
   --  @method
   --  ------------------------------
   procedure Delete (Bean    : in out Upload_Bean;
                     Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      Manager : constant Services.Storage_Service_Access := Bean.Module.Get_Storage_Manager;
   begin
      Manager.Delete (Bean);
      Outcome := Ada.Strings.Unbounded.To_Unbounded_String ("success");
   end Delete;

   --  ------------------------------
   --  Get the value identified by the name.
   --  ------------------------------
   overriding
   function Get_Value (From : in Folder_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if ADO.Objects.Is_Null (From) then
         return Util.Beans.Objects.Null_Object;
      else
         return AWA.Storages.Models.Storage_Folder_Ref (From).Get_Value (Name);
      end if;
   end Get_Value;

   --  ------------------------------
   --  Set the value identified by the name.
   --  ------------------------------
   overriding
   procedure Set_Value (From  : in out Folder_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if Name = "name" then
         From.Set_Name (Util.Beans.Objects.To_String (Value));
      end if;
   end Set_Value;

   --  ------------------------------
   --  Create or save the folder.
   --  ------------------------------
   procedure Save (Bean    : in out Folder_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      Manager : constant Services.Storage_Service_Access := Bean.Module.Get_Storage_Manager;
   begin
      Manager.Save_Folder (Bean);
      Outcome := Ada.Strings.Unbounded.To_Unbounded_String ("success");
   end Save;

   --  ------------------------------
   --  Load the folder instance.
   --  ------------------------------
   procedure Load_Folder (Storage : in Storage_List_Bean) is
      use AWA.Storages.Models;
      use AWA.Services;
      use type Ada.Containers.Count_Type;

      Manager : constant Services.Storage_Service_Access := Storage.Module.Get_Storage_Manager;
   begin
      Load_Folders (Storage);
      if Storage.Folder_List.List.Length > 0 then
         Manager.Load_Folder (Storage.Folder_Bean.all,
                                      Storage.Folder_List.List.Element (0).Id);

      end if;
      Storage.Flags (INIT_FOLDER) := True;
   end Load_Folder;

   --  ------------------------------
   --  Load the list of folders.
   --  ------------------------------
   procedure Load_Folders (Storage : in Storage_List_Bean) is
      use AWA.Storages.Models;
      use AWA.Services;
      use type ADO.Identifier;

      Ctx     : constant Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      User    : constant ADO.Identifier := Ctx.Get_User_Identifier;
      Session : ADO.Sessions.Session := Storage.Module.Get_Session;
      Query   : ADO.Queries.Context;
   begin
      Query.Set_Query (AWA.Storages.Models.Query_Storage_Folder_List);
      Query.Bind_Param ("user_id", User);
      ADO.Sessions.Entities.Bind_Param (Query, "table",
                                        AWA.Workspaces.Models.WORKSPACE_TABLE, Session);
      AWA.Storages.Models.List (Storage.Folder_List_Bean.all, Session, Query);
      Storage.Flags (INIT_FOLDER_LIST) := True;
   end Load_Folders;

   --  ------------------------------
   --  Load the list of files associated with the current folder.
   --  ------------------------------
   procedure Load_Files (Storage : in Storage_List_Bean) is
      use AWA.Storages.Models;
      use AWA.Services;

      Ctx       : constant Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      User      : constant ADO.Identifier := Ctx.Get_User_Identifier;
      Session   : ADO.Sessions.Session := Storage.Module.Get_Session;
      Query     : ADO.Queries.Context;
   begin
      if not Storage.Init_Flags (INIT_FOLDER) then
         Load_Folder (Storage);
      end if;
      Query.Set_Query (AWA.Storages.Models.Query_Storage_List);
      Query.Bind_Param ("user_id", User);
      ADO.Sessions.Entities.Bind_Param (Query, "table",
                                        AWA.Workspaces.Models.WORKSPACE_TABLE, Session);
      if Storage.Folder_Bean.Is_Null then
         Query.Bind_Null_Param ("folder_id");
      else
         Query.Bind_Param ("folder_id", Storage.Folder_Bean.Get_Id);
      end if;
      AWA.Storages.Models.List (Storage.Files_List_Bean.all, Session, Query);
      Storage.Flags (INIT_FILE_LIST) := True;
   end Load_Files;

   --  ------------------------------
   --  Set the value identified by the name.
   --  ------------------------------
   overriding
   procedure Set_Value (From  : in out Storage_List_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
      Manager : constant Services.Storage_Service_Access := From.Module.Get_Storage_Manager;
   begin
      if Name = "folderId" and not Util.Beans.Objects.Is_Empty (Value) then
         Manager.Load_Folder (From.Folder, ADO.Identifier (Util.Beans.Objects.To_Integer (Value)));
         From.Flags (INIT_FOLDER) := True;
      end if;
   end Set_Value;

   overriding
   function Get_Value (List : in Storage_List_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = "files" then
         if not List.Init_Flags (INIT_FILE_LIST) then
            Load_Files (List);
         end if;
         return Util.Beans.Objects.To_Object (Value   => List.Files_List_Bean,
                                              Storage => Util.Beans.Objects.STATIC);

      elsif Name = "folders" then
         if not List.Init_Flags (INIT_FOLDER_LIST) then
            Load_Folders (List);
         end if;
         return Util.Beans.Objects.To_Object (Value   => List.Folder_List_Bean,
                                              Storage => Util.Beans.Objects.STATIC);

      elsif Name = "folder" then
         if not List.Init_Flags (INIT_FOLDER) then
            Load_Folder (List);
         end if;
         if List.Folder_Bean.Is_Null then
            return Util.Beans.Objects.Null_Object;
         end if;
         return Util.Beans.Objects.To_Object (Value   => List.Folder_Bean,
                                              Storage => Util.Beans.Objects.STATIC);

      else
         return Util.Beans.Objects.Null_Object;
      end if;
   end Get_Value;

   --  ------------------------------
   --  Create the Folder_List_Bean bean instance.
   --  ------------------------------
   function Create_Folder_List_Bean (Module : in AWA.Storages.Modules.Storage_Module_Access)
                                     return Util.Beans.Basic.Readonly_Bean_Access is
      use AWA.Storages.Models;
      use AWA.Services;
      use type ADO.Identifier;

      Ctx   : constant Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      User  : constant ADO.Identifier := Ctx.Get_User_Identifier;

      Object  : constant Folder_Info_List_Bean_Access := new Folder_Info_List_Bean;
      Session : ADO.Sessions.Session := Module.Get_Session;
      Query   : ADO.Queries.Context;
   begin
      Query.Set_Query (AWA.Storages.Models.Query_Storage_Folder_List);
      Query.Bind_Param ("user_id", User);
      ADO.Sessions.Entities.Bind_Param (Query, "table",
                                        AWA.Workspaces.Models.WORKSPACE_TABLE, Session);
      AWA.Storages.Models.List (Object.all, Session, Query);
      return Object.all'Access;
   end Create_Folder_List_Bean;

   --  ------------------------------
   --  Create the Storage_List_Bean bean instance.
   --  ------------------------------
   function Create_Storage_List_Bean (Module : in AWA.Storages.Modules.Storage_Module_Access)
                                      return Util.Beans.Basic.Readonly_Bean_Access is
      Object    : constant Storage_List_Bean_Access := new Storage_List_Bean;
   begin
      Object.Module           := Module;
      Object.Folder_Bean      := Object.Folder'Access;
      Object.Folder_List_Bean := Object.Folder_List'Access;
      Object.Files_List_Bean  := Object.Files_List'Access;
      Object.Flags            := Object.Init_Flags'Access;
      return Object.all'Access;
   end Create_Storage_List_Bean;

end AWA.Storages.Beans;
