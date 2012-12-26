-----------------------------------------------------------------------
--  awa-storages-beans -- Storage Ada Beans
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
with ADO;
with ADO.Queries;
with ADO.Sessions;
with ADO.Objects;
with ADO.Sessions.Entities;

with AWA.Helpers.Requests;
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
      end if;
      return AWA.Storages.Models.Storage_Ref (From).Get_Value (Name);
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
         Manager.Load_Folder (Folder, ADO.Identifier (Util.Beans.Objects.To_Integer (Value)));
         From.Set_Folder (Folder);
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

   overriding
   function Get_Value (List : in Storage_List_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      return Models.Storage_Info_List_Bean (List).Get_Value (Name);
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
      use AWA.Storages.Models;
      use AWA.Services;
      use type ADO.Identifier;

      Ctx       : constant Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      User      : constant ADO.Identifier := Ctx.Get_User_Identifier;
      Object    : constant Storage_List_Bean_Access := new Storage_List_Bean;
      Session   : ADO.Sessions.Session := Module.Get_Session;
      Query     : ADO.Queries.Context;
      Folder_Id : constant ADO.Identifier := Helpers.Requests.Get_Parameter (FOLDER_ID_PARAMETER);
   begin
      Query.Set_Query (AWA.Storages.Models.Query_Storage_List);
      Query.Bind_Param ("user_id", User);
      ADO.Sessions.Entities.Bind_Param (Query, "table",
                                        AWA.Workspaces.Models.WORKSPACE_TABLE, Session);
      if Folder_Id = ADO.NO_IDENTIFIER then
         Query.Bind_Null_Param ("folder_id");
      else
         Query.Bind_Param ("folder_id", Folder_Id);
      end if;
      AWA.Storages.Models.List (Object.all, Session, Query);
      return Object.all'Access;
   end Create_Storage_List_Bean;

end AWA.Storages.Beans;
