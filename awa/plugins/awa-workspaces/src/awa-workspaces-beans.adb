-----------------------------------------------------------------------
--  awa-workspaces-beans -- Beans for module workspaces
--  Copyright (C) 2011, 2012, 2017 Stephane Carrez
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

with ASF.Events.Faces.Actions;

with ADO.Utils;
with ADO.Sessions.Entities;
with ADO.Sessions;
with ADO.Queries;
with ADO.Datasets;

with AWA.Services.Contexts;
with AWA.Workspaces.Models;
with AWA.Events.Action_Method;
with AWA.Services.Contexts;
package body AWA.Workspaces.Beans is

   package ASC renames AWA.Services.Contexts;

   overriding
   procedure Load (Bean : in out Invitation_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
   begin
      null;
   end Load;

   overriding
   procedure Accept_Invitation (Bean    : in out Invitation_Bean;
                                Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
   begin
      null;
   end Accept_Invitation;

   overriding
   procedure Send (Bean    : in out Invitation_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
   begin
      null;
   end Send;

   --  ------------------------------
   --  Example of action method.
   --  ------------------------------
   procedure Action (Bean    : in out Workspaces_Bean;
                     Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
   begin
      null;
   end Action;

   --  ------------------------------
   --  Event action called to create the workspace when the given event is posted.
   --  ------------------------------
   procedure Create (Bean  : in out Workspaces_Bean;
                     Event : in AWA.Events.Module_Event'Class) is
      pragma Unreferenced (Bean, Event);

      WS  : AWA.Workspaces.Models.Workspace_Ref;
      Ctx : constant AWA.Services.Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      DB  : ADO.Sessions.Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
   begin
      Ctx.Start;
      AWA.Workspaces.Modules.Get_Workspace (Session   => DB,
                                            Context   => Ctx,
                                            Workspace => WS);
      Ctx.Commit;
   end Create;

   package Action_Binding is
     new ASF.Events.Faces.Actions.Action_Method.Bind (Bean   => Workspaces_Bean,
                                                      Method => Action,
                                                      Name   => "action");

   package Create_Binding is
      new AWA.Events.Action_Method.Bind (Name   => "create",
                                         Bean   => Workspaces_Bean,
                                         Method => Create);

   Workspaces_Bean_Binding : aliased constant Util.Beans.Methods.Method_Binding_Array
     := (Action_Binding.Proxy'Access, Create_Binding.Proxy'Access);

   --  ------------------------------
   --  Get the value identified by the name.
   --  ------------------------------
   overriding
   function Get_Value (From : in Workspaces_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = "count" then
         return Util.Beans.Objects.To_Object (From.Count);
      else
         return Util.Beans.Objects.Null_Object;
      end if;
   end Get_Value;

   --  ------------------------------
   --  Set the value identified by the name.
   --  ------------------------------
   overriding
   procedure Set_Value (From  : in out Workspaces_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if Name = "count" then
         From.Count := Util.Beans.Objects.To_Integer (Value);
      end if;
   end Set_Value;

   --  ------------------------------
   --  This bean provides some methods that can be used in a Method_Expression
   --  ------------------------------
   overriding
   function Get_Method_Bindings (From : in Workspaces_Bean)
                                 return Util.Beans.Methods.Method_Binding_Array_Access is
      pragma Unreferenced (From);
   begin
      return Workspaces_Bean_Binding'Access;
   end Get_Method_Bindings;

   --  ------------------------------
   --  Create the Workspaces_Bean bean instance.
   --  ------------------------------
   function Create_Workspaces_Bean (Module : in AWA.Workspaces.Modules.Workspace_Module_Access)
      return Util.Beans.Basic.Readonly_Bean_Access is
      Object : constant Workspaces_Bean_Access := new Workspaces_Bean;
   begin
      Object.Module := Module;
      return Object.all'Access;
   end Create_Workspaces_Bean;

   --  ------------------------------
   --  Load the list of members.
   --  ------------------------------
   overriding
   procedure Load (Into    : in out Member_List_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      Ctx         : constant ASC.Service_Context_Access := ASC.Current;
      User        : constant ADO.Identifier := Ctx.Get_User_Identifier;
      Session     : ADO.Sessions.Session := Into.Module.Get_Session;
      Query       : ADO.Queries.Context;
      Count_Query : ADO.Queries.Context;
      First       : constant Natural  := (Into.Page - 1) * Into.Page_Size;
   begin
      Query.Set_Query (AWA.Workspaces.Models.Query_Workspace_Member_List);
      Count_Query.Set_Count_Query (AWA.Workspaces.Models.Query_Workspace_Member_List);
--        ADO.Sessions.Entities.Bind_Param (Params  => Query,
--                                          Name    => "page_table",
--                                          Table   => AWA.Wikis.Models.WIKI_PAGE_TABLE,
--                                          Session => Session);
      Query.Bind_Param (Name => "user_id", Value => User);
      Count_Query.Bind_Param (Name => "user_id", Value => User);
      AWA.Workspaces.Models.List (Into.Members, Session, Query);
      Into.Count := ADO.Datasets.Get_Count (Session, Count_Query);
   end Load;

end AWA.Workspaces.Beans;
