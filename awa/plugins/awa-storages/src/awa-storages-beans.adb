-----------------------------------------------------------------------
--  awa-storages-beans -- Storage Ada Beans
--  Copyright (C) 2012, 2013, 2016, 2018, 2019, 2020, 2022 Stephane Carrez
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

with Util.Log.Loggers;
with Util.Strings;

with ADO.Utils;
with ADO.Queries;
with ADO.Sessions;
with ADO.Objects;

with AWA.Permissions;
with AWA.Helpers.Requests;
with AWA.Services.Contexts;
with AWA.Storages.Services;
package body AWA.Storages.Beans is

   use Ada.Strings.Unbounded;
   use type ADO.Identifier;

   package ASC renames AWA.Services.Contexts;

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("AWA.Storages.Beans");

   --  ------------------------------
   --  Get the value identified by the name.
   --  ------------------------------
   overriding
   function Get_Value (From : in Upload_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = "folderId" then
         return ADO.Utils.To_Object (From.Folder_Id);
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
      Found   : Boolean;
      Id      : ADO.Identifier;
   begin
      if Name = "folderId" then
         From.Folder_Id := ADO.Utils.To_Identifier (Value);
         Manager.Load_Folder (Folder, From.Folder_Id);
         From.Set_Folder (Folder);
      elsif Name = "id" and then not Util.Beans.Objects.Is_Empty (Value) then
         Id := ADO.Utils.To_Identifier (Value);
         if Id /= ADO.NO_IDENTIFIER then
            Manager.Load_Storage (From, Id);
         end if;
      elsif Name = "name" then
         Folder := Models.Storage_Folder_Ref (From.Get_Folder);
         Manager.Load_Storage (From, From.Folder_Id, Util.Beans.Objects.To_String (Value), Found);
         if not Found then
            From.Set_Name (Util.Beans.Objects.To_String (Value));
         end if;
         From.Set_Folder (Folder);
      end if;
   end Set_Value;

   --  ------------------------------
   --  Save the uploaded file in the storage service.
   --  ------------------------------
   overriding
   procedure Save_Part (Bean : in out Upload_Bean;
                        Part : in ASF.Parts.Part'Class) is
      Manager : constant Services.Storage_Service_Access := Bean.Module.Get_Storage_Manager;
      Name    : constant String := Bean.Get_Name;
   begin
      if Name'Length = 0 then
         Bean.Set_Name (Part.Get_Name);
      end if;
      Bean.Set_Mime_Type (Part.Get_Content_Type);
      Bean.Set_File_Size (Part.Get_Size);
      Manager.Save (Bean, Part);

   exception
      when AWA.Permissions.NO_PERMISSION =>
         Bean.Error := True;
         Log.Error ("Saving document is refused by the permission controller");
         raise;

      when E : others =>
         Bean.Error := True;
         Log.Error ("Exception when saving the document", E);
         raise;

   end Save_Part;

   --  ------------------------------
   --  Upload the file.
   --  ------------------------------
   overriding
   procedure Upload (Bean    : in out Upload_Bean;
                     Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
   begin
      Outcome := To_Unbounded_String (if Bean.Error then "failure" else "success");
   end Upload;

   --  ------------------------------
   --  Delete the file.
   --  ------------------------------
   overriding
   procedure Delete (Bean    : in out Upload_Bean;
                     Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      Manager : constant Services.Storage_Service_Access := Bean.Module.Get_Storage_Manager;
   begin
      Manager.Delete (Bean);
      Outcome := Ada.Strings.Unbounded.To_Unbounded_String ("success");
   end Delete;

   --  ------------------------------
   --  Publish the file.
   --  ------------------------------
   overriding
   procedure Publish (Bean    : in out Upload_Bean;
                      Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      Manager : constant Services.Storage_Service_Access := Bean.Module.Get_Storage_Manager;
      Id      : constant ADO.Identifier := Helpers.Requests.Get_Parameter ("id");
      Value   : constant Util.Beans.Objects.Object := Helpers.Requests.Get_Parameter ("status");
   begin
      Manager.Publish (Id, Util.Beans.Objects.To_Boolean (Value), Bean);
      Outcome := Ada.Strings.Unbounded.To_Unbounded_String ("success");
   end Publish;

   --  ------------------------------
   --  Create the Upload_Bean bean instance.
   --  ------------------------------
   function Create_Upload_Bean (Module : in AWA.Storages.Modules.Storage_Module_Access)
                                return Util.Beans.Basic.Readonly_Bean_Access is
      Result : constant Upload_Bean_Access := new Upload_Bean;
   begin
      Result.Module := Module;
      return Result.all'Access;
   end Create_Upload_Bean;

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
   overriding
   procedure Save (Bean    : in out Folder_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      Manager : constant Services.Storage_Service_Access := Bean.Module.Get_Storage_Manager;
   begin
      Manager.Save_Folder (Bean);
      Outcome := Ada.Strings.Unbounded.To_Unbounded_String ("success");
   end Save;

   --  ------------------------------
   --  Create the Folder_Bean bean instance.
   --  ------------------------------
   function Create_Folder_Bean (Module : in AWA.Storages.Modules.Storage_Module_Access)
                                return Util.Beans.Basic.Readonly_Bean_Access is
      Result : constant Folder_Bean_Access := new Folder_Bean;
   begin
      Result.Module := Module;
      return Result.all'Access;
   end Create_Folder_Bean;

   --  ------------------------------
   --  Load the folder instance.
   --  ------------------------------
   procedure Load_Folder (Storage : in out Storage_List_Bean) is
      use AWA.Storages.Models;
      use type Ada.Containers.Count_Type;

      Manager : constant Services.Storage_Service_Access := Storage.Module.Get_Storage_Manager;
   begin
      if Storage.Folder_Id /= ADO.NO_IDENTIFIER then
         Manager.Load_Folder (Storage.Folder_Bean.all,
                              Storage.Folder_Id);

      elsif Storage.Folder_List.List.Length > 0 then
         Manager.Load_Folder (Storage.Folder_Bean.all,
                              Storage.Folder_List.List.Element (1).Id);

      end if;
      Storage.Flags (INIT_FOLDER) := True;
   end Load_Folder;

   --  ------------------------------
   --  Load the list of folders.
   --  ------------------------------
   procedure Load_Folders (Storage : in out Storage_List_Bean) is
      use AWA.Storages.Models;
      use AWA.Services;

      Ctx     : constant Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      User    : constant ADO.Identifier := Ctx.Get_User_Identifier;
      Session : ADO.Sessions.Session := ASC.Get_Session (Ctx);
      Query   : ADO.Queries.Context;
   begin
      Query.Set_Query (AWA.Storages.Models.Query_Storage_Folder_List);
      Query.Bind_Param ("user_id", User);
      AWA.Storages.Models.List (Storage.Folder_List_Bean.all, Session, Query);
      Storage.Flags (INIT_FOLDER_LIST) := True;
   end Load_Folders;

   --  ------------------------------
   --  Load the list of files associated with the current folder.
   --  ------------------------------
   procedure Load_Files (Storage : in out Storage_List_Bean) is
      use AWA.Storages.Models;
      use AWA.Services;

      Ctx       : constant Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      User      : constant ADO.Identifier := Ctx.Get_User_Identifier;
      Session   : ADO.Sessions.Session := ASC.Get_Session (Ctx);
      Query     : ADO.Queries.Context;
   begin
      if not Storage.Init_Flags (INIT_FOLDER) then
         Load_Folder (Storage);
      end if;
      Query.Set_Query (AWA.Storages.Models.Query_Storage_List);
      Query.Bind_Param ("user_id", User);
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
   begin
      if Name = "folderId" and then not Util.Beans.Objects.Is_Empty (Value) then
         From.Folder_Id := ADO.Utils.To_Identifier (Value);
      end if;
   end Set_Value;

   overriding
   function Get_Value (List : in Storage_List_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = "files" then
         return Util.Beans.Objects.To_Object (Value   => List.Files_List_Bean,
                                              Storage => Util.Beans.Objects.STATIC);

      elsif Name = "folders" then
         return Util.Beans.Objects.To_Object (Value   => List.Folder_List_Bean,
                                              Storage => Util.Beans.Objects.STATIC);

      elsif Name = "folder" then
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
   --  Load the files and folder information.
   --  ------------------------------
   overriding
   procedure Load (List    : in out Storage_List_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      pragma Unreferenced (Outcome);
   begin
      Storage_List_Bean'Class (List).Load_Folders;
      Storage_List_Bean'Class (List).Load_Folder;
      Storage_List_Bean'Class (List).Load_Files;
   end Load;

   --  ------------------------------
   --  Create the Folder_List_Bean bean instance.
   --  ------------------------------
   function Create_Folder_List_Bean (Module : in AWA.Storages.Modules.Storage_Module_Access)
                                     return Util.Beans.Basic.Readonly_Bean_Access is
      pragma Unreferenced (Module);
      use AWA.Storages.Models;
      use AWA.Services;

      Ctx   : constant Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      User  : constant ADO.Identifier := Ctx.Get_User_Identifier;

      Object  : constant Folder_Info_List_Bean_Access := new Folder_Info_List_Bean;
      Session : ADO.Sessions.Session := ASC.Get_Session (Ctx);
      Query   : ADO.Queries.Context;
   begin
      Query.Set_Query (AWA.Storages.Models.Query_Storage_Folder_List);
      Query.Bind_Param ("user_id", User);
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

   --  ------------------------------
   --  Returns true if the given mime type can be displayed by a browser.
   --  Mime types: application/pdf, text/*, image/*
   --  ------------------------------
   function Is_Browser_Visible (Mime_Type : in String) return Boolean is
   begin
      if Mime_Type = "application/pdf" then
         return True;
      end if;
      if Util.Strings.Starts_With (Mime_Type, Prefix => "text/") then
         return True;
      end if;
      if Util.Strings.Starts_With (Mime_Type, Prefix => "image/") then
         return True;
      end if;
      return False;
   end Is_Browser_Visible;

   overriding
   function Get_Value (From : in Storage_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = "isBrowserVisible" then
         return Util.Beans.Objects.To_Object (Is_Browser_Visible (To_String (From.Mime_Type)));
      else
         return AWA.Storages.Models.Storage_Bean (From).Get_Value (Name);
      end if;
   end Get_Value;

   overriding
   procedure Load (Into    : in out Storage_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      Ctx     : constant ASC.Service_Context_Access := ASC.Current;
      User    : constant ADO.Identifier := Ctx.Get_User_Identifier;
      Session : ADO.Sessions.Session := ASC.Get_Session (Ctx);
      Query   : ADO.Queries.Context;
   begin
      if Into.Id = ADO.NO_IDENTIFIER then
         Outcome := Ada.Strings.Unbounded.To_Unbounded_String ("not-found");
         return;
      end if;

      --  Get the image information.
      Query.Set_Query (AWA.Storages.Models.Query_Storage_Info);
      Query.Bind_Param (Name => "user_id", Value => User);
      Query.Bind_Param (Name => "file_id", Value => Into.Id);
      Into.Load (Session, Query);

   exception
      when ADO.Objects.NOT_FOUND =>
         Outcome := Ada.Strings.Unbounded.To_Unbounded_String ("not-found");

   end Load;

   --  ------------------------------
   --  Create the Storage_Bean bean instance.
   --  ------------------------------
   function Create_Storage_Bean (Module : in AWA.Storages.Modules.Storage_Module_Access)
                               return Util.Beans.Basic.Readonly_Bean_Access is
      Object    : constant Storage_Bean_Access := new Storage_Bean;
   begin
      Object.Module  := Module;
      Object.Id      := ADO.NO_IDENTIFIER;
      return Object.all'Access;
   end Create_Storage_Bean;

end AWA.Storages.Beans;
