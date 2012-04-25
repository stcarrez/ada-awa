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

with Util.Log.Loggers;

with ADO.Objects;
with ADO.Statements;
with ADO.Sessions.Entities;

with AWA.Services.Contexts;
with AWA.Workspaces.Models;
with AWA.Workspaces.Module;
with AWA.Permissions;
package body AWA.Storages.Services is

   use AWA.Services;

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("AWA.Storages.Services");

   --  ------------------------------
   --  Save the data object contained in the <b>Data</b> part element into the
   --  target storage represented by <b>Into</b>.
   --  ------------------------------
   procedure Save (Service : in Storage_Service;
                   Into    : in out AWA.Storages.Models.Storage_Ref'Class;
                   Data    : in ASF.Parts.Part'Class) is
      use type AWA.Storages.Models.Storage_Type;

      Ctx    : constant Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      DB     : ADO.Sessions.Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
      Store  : AWA.Storages.Models.Storage_Data_Ref;
   begin
      Ctx.Start;
      if not Into.Is_Inserted then
         declare
            Workspace : AWA.Workspaces.Models.Workspace_Ref;
         begin
            AWA.Workspaces.Module.Get_Workspace (DB, Ctx, Workspace);
            Into.Set_Workspace (Workspace);
            Into.Set_Create_Date (Ada.Calendar.Clock);
         end;
      end if;
      if Into.Get_Storage = AWA.Storages.Models.DATABASE then
         Service.Save (DB, Data.Get_Local_Filename, Store);
         Into.Set_Store_Data (Store);
      end if;
      Into.Save (DB);
      Ctx.Commit;
   end Save;

   --  ------------------------------
   --  Save the file pointed to by the <b>Path</b> string in the storage
   --  object represented by <b>Into</b> and managed by the storage service.
   --  ------------------------------
   procedure Save (Service : in Storage_Service;
                   Into    : in out AWA.Storages.Models.Storage_Ref'Class;
                   Path    : in String) is
      use type AWA.Storages.Models.Storage_Type;

      Ctx       : constant Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      DB        : ADO.Sessions.Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
      Data      : AWA.Storages.Models.Storage_Data_Ref;
      Workspace : AWA.Workspaces.Models.Workspace_Ref;
   begin
      Log.Info ("Save {0} in storage space", Path);

      if not Into.Is_Null then
         Workspace := AWA.Workspaces.Models.Workspace_Ref (Into.Get_Workspace);
      end if;
      if Workspace.Is_Null then
         AWA.Workspaces.Module.Get_Workspace (DB, Ctx, Workspace);
         Into.Set_Workspace (Workspace);
      end if;

      --  Check that the user has the create storage permission on the given workspace.
      AWA.Permissions.Check (Permission => ACL_Create_Storage.Permission,
                             Entity     => Workspace.Get_Id);

      Ctx.Start;
      if not Into.Is_Inserted then
         Into.Set_Create_Date (Ada.Calendar.Clock);
      end if;
      if Into.Get_Storage = AWA.Storages.Models.DATABASE then
         Service.Save (DB, Path, Data);
         Into.Set_Store_Data (Data);
      end if;
      Into.Save (DB);
      Ctx.Commit;
   end Save;

   --  ------------------------------
   --  Save the file whose path is specified in <b>Path</b> in the data object
   --  refered to by <b>Data</b>.
   --  ------------------------------
   procedure Save (Service : in Storage_Service;
                   Session : in out ADO.Sessions.Master_Session;
                   Path    : in String;
                   Data    : in out AWA.Storages.Models.Storage_Data_Ref) is
      pragma Unreferenced (Service);

      Blob : constant ADO.Blob_Ref := ADO.Create_Blob (Path);
   begin
      Data.Set_Data (Blob);
      Data.Save (Session);
   end Save;

   --  Load the storage content identified by <b>From</b> in a local file
   --  that will be identified by <b>Into</b>.
   procedure Load (Service : in Storage_Service;
                   From    : in AWA.Storages.Models.Storage_Ref'Class;
                   Into    : in out AWA.Storages.Models.Store_Local_Ref'Class) is
   begin
      null;
   end Load;

   --  ------------------------------
   --  Load the storage content identified by <b>From</b> into the blob descriptor <b>Into</b>.
   --  Raises the <b>NOT_FOUND</b> exception if there is no such storage.
   --  ------------------------------
   procedure Load (Service : in Storage_Service;
                   From    : in ADO.Identifier;
                   Into    : out ADO.Blob_Ref) is
      pragma Unreferenced (Service);

      Ctx   : constant Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      User  : constant ADO.Identifier := Ctx.Get_User_Identifier;
      DB    : constant ADO.Sessions.Session := AWA.Services.Contexts.Get_Session (Ctx);
      Query : ADO.Statements.Query_Statement
        := DB.Create_Statement (Models.Query_Storage_Get_Data);
   begin
      Query.Bind_Param ("store_id", From);
      Query.Bind_Param ("user_id", User);
      ADO.Sessions.Entities.Bind_Param (Query, "table",
                                        AWA.Workspaces.Models.WORKSPACE_TABLE'Access, DB);

      Query.Execute;
      Into := Query.Get_Result_Blob;
   end Load;

   --  ------------------------------
   --  Deletes the storage instance.
   --  ------------------------------
   procedure Delete (Service : in Storage_Service;
                     Storage : in out AWA.Storages.Models.Storage_Ref'Class) is
      pragma Unreferenced (Service);

      Ctx : constant Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      DB  : ADO.Sessions.Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
      Id  : constant ADO.Identifier := ADO.Objects.Get_Value (Storage.Get_Key);
   begin
      Log.Info ("Delete storage {0}", ADO.Identifier'Image (Id));

      --  Check that the user has the delete storage permission on the given workspace.
      AWA.Permissions.Check (Permission => ACL_Delete_Storage.Permission,
                             Entity     => Id);

      Ctx.Start;
      Storage.Delete (DB);
      Ctx.Commit;
   end Delete;

   --  ------------------------------
   --  Deletes the storage instance.
   --  ------------------------------
   procedure Delete (Service : in Storage_Service;
                     Storage : in ADO.Identifier) is
      pragma Unreferenced (Service);

      Ctx  : constant Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      DB   : ADO.Sessions.Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
      S    : AWA.Storages.Models.Storage_Ref;
      Query : ADO.Statements.Query_Statement
        := DB.Create_Statement (AWA.Storages.Models.Query_Storage_Delete_Local);
   begin
      Log.Info ("Delete storage {0}", ADO.Identifier'Image (Storage));

      --  Check that the user has the delete storage permission on the given workspace.
      AWA.Permissions.Check (Permission => ACL_Delete_Storage.Permission,
                             Entity     => Storage);

      S.Set_Id (Storage);
      Ctx.Start;
      --  Delete the local storage instances.
      Query.Bind_Param ("store_id", Storage);
      Query.Execute;
      S.Delete (DB);
      Ctx.Commit;
   end Delete;

end AWA.Storages.Services;
