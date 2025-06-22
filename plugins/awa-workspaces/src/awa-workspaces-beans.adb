-----------------------------------------------------------------------
--  awa-workspaces-beans -- Beans for module workspaces
--  Copyright (C) 2011, 2012, 2017, 2018, 2022, 2023 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with ASF.Contexts.Flash;
with ASF.Contexts.Faces;
with ASF.Applications.Messages.Factory;

with ADO.Utils;
with ADO.Sessions;
with ADO.Queries;
with ADO.Datasets;

with AWA.Services.Contexts;
with AWA.Events.Action_Method;
package body AWA.Workspaces.Beans is

   use ASF.Applications;

   package ASC renames AWA.Services.Contexts;
   use type ASC.Service_Context_Access;

   --  ------------------------------
   --  Get the value identified by the name.
   --  ------------------------------
   overriding
   function Get_Value (From : in Member_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      return AWA.Workspaces.Models.Member_Bean (From).Get_Value (Name);
   end Get_Value;

   --  ------------------------------
   --  Set the value identified by the name
   --  ------------------------------
   overriding
   procedure Set_Value (Item  : in out Member_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if Name = "id" and then not Util.Beans.Objects.Is_Empty (Value) then
         Item.Set_Id (ADO.Utils.To_Identifier (Value));
      else
         AWA.Workspaces.Models.Member_Bean (Item).Set_Value (Name, Value);
      end if;
   end Set_Value;

   overriding
   procedure Load (Bean : in out Member_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      pragma Unreferenced (Outcome);
   begin
      null;
   end Load;

   overriding
   procedure Delete (Bean : in out Member_Bean;
                     Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      pragma Unreferenced (Outcome);
   begin
      Bean.Module.Delete_Member (Bean.Get_Id);
   end Delete;

   --  ------------------------------
   --  Create the Member_Bean bean instance.
   --  ------------------------------
   function Create_Member_Bean (Module : in AWA.Workspaces.Modules.Workspace_Module_Access)
                                return Util.Beans.Basic.Readonly_Bean_Access is
      Object  : constant Member_Bean_Access := new Member_Bean;
   begin
      Object.Module := Module;
      return Object.all'Access;
   end Create_Member_Bean;

   --  ------------------------------
   --  Get the value identified by the name.
   --  ------------------------------
   overriding
   function Get_Value (From : in Invitation_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = "inviter" then
         return From.Inviter.Get_Value ("name");
      else
         return AWA.Workspaces.Models.Invitation_Bean (From).Get_Value (Name);
      end if;
   end Get_Value;

   overriding
   procedure Load (Bean : in out Invitation_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      Ctx : constant ASC.Service_Context_Access := ASC.Current;
      Key : constant String := Ada.Strings.Unbounded.To_String (Bean.Key);
   begin
      Bean.Module.Load_Invitation (Key        => Key,
                                   Invitation => Bean,
                                   Inviter    => Bean.Inviter);
      if Ctx /= null then
         declare
            User  : constant AWA.Users.Models.User_Ref := Ctx.Get_User;
            Fctx  : constant ASF.Contexts.Faces.Faces_Context_Access := ASF.Contexts.Faces.Current;
            Flash : constant ASF.Contexts.Faces.Flash_Context_Access := Fctx.Get_Flash;
         begin
            --  If the user is logged, accept the invitation
            if not User.Is_Null then
               Bean.Module.Accept_Invitation (Key => Key);
               Flash.Set_Keep_Messages (True);
               Messages.Factory.Add_Message (Fctx.all, "workspaces.workspace_welcome_message",
                                             Messages.INFO);
               Outcome := Ada.Strings.Unbounded.To_Unbounded_String ("success-key");
            end if;
         end;
      end if;

   exception
      when others =>
         Outcome := Ada.Strings.Unbounded.To_Unbounded_String ("not-found");
   end Load;

   overriding
   procedure Accept_Invitation (Bean    : in out Invitation_Bean;
                                Event   : in AWA.Events.Module_Event'Class) is
      Key   : constant String := Event.Get_Parameter ("key");
      Ctx   : constant ASF.Contexts.Faces.Faces_Context_Access := ASF.Contexts.Faces.Current;
      Flash : constant ASF.Contexts.Faces.Flash_Context_Access := Ctx.Get_Flash;
   begin
      Bean.Module.Accept_Invitation (Key => Key);
      Flash.Set_Keep_Messages (True);
      Messages.Factory.Add_Message (Ctx.all, "workspaces.workspace_welcome_message",
                                    Messages.INFO);
   end Accept_Invitation;

   overriding
   procedure Send (Bean    : in out Invitation_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      pragma Unreferenced (Outcome);

      Ctx   : constant ASF.Contexts.Faces.Faces_Context_Access := ASF.Contexts.Faces.Current;
      Flash : constant ASF.Contexts.Faces.Flash_Context_Access := Ctx.Get_Flash;
   begin
      Bean.Module.Send_Invitation (Bean);
      Flash.Set_Keep_Messages (True);
      Messages.Factory.Add_Message (Ctx.all, "workspaces.workspace_invitation_sent",
                                    Messages.INFO);
   end Send;

   --  ------------------------------
   --  Create the Invitation_Bean bean instance.
   --  ------------------------------
   function Create_Invitation_Bean (Module : in AWA.Workspaces.Modules.Workspace_Module_Access)
                                    return Util.Beans.Basic.Readonly_Bean_Access is
      Object  : constant Invitation_Bean_Access := new Invitation_Bean;
   begin
      Object.Module := Module;
      return Object.all'Access;
   end Create_Invitation_Bean;

   --  ------------------------------
   --  Event action called to create the workspace when the given event is posted.
   --  ------------------------------
   procedure Create (Bean  : in out Workspaces_Bean;
                     Event : in AWA.Events.Module_Event'Class) is
      pragma Unreferenced (Event);

      WS  : AWA.Workspaces.Models.Workspace_Ref;
   begin
      Bean.Module.Create_Workspace (WS);
   end Create;

   package Create_Binding is
      new AWA.Events.Action_Method.Bind (Name   => "create",
                                         Bean   => Workspaces_Bean,
                                         Method => Create);

   Workspaces_Bean_Binding : aliased constant Util.Beans.Methods.Method_Binding_Array
     := (1 => Create_Binding.Proxy'Access);

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
   --  Get the value identified by the name.
   --  ------------------------------
   overriding
   function Get_Value (From : in Member_List_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = "members" then
         return Util.Beans.Objects.To_Object (Value   => From.Members_Bean,
                                              Storage => Util.Beans.Objects.STATIC);
      else
         return AWA.Workspaces.Models.Member_List_Bean (From).Get_Value (Name);
      end if;
   end Get_Value;

   --  ------------------------------
   --  Load the list of members.
   --  ------------------------------
   overriding
   procedure Load (Into    : in out Member_List_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      pragma Unreferenced (Outcome);

      Ctx         : constant ASC.Service_Context_Access := ASC.Current;
      User        : constant ADO.Identifier := Ctx.Get_User_Identifier;
      Session     : ADO.Sessions.Session := Into.Module.Get_Session;
      Query       : ADO.Queries.Context;
      Count_Query : ADO.Queries.Context;
   begin
      Query.Set_Query (AWA.Workspaces.Models.Query_Workspace_Member_List);
      Count_Query.Set_Count_Query (AWA.Workspaces.Models.Query_Workspace_Member_List);
      Query.Bind_Param (Name => "user_id", Value => User);
      Count_Query.Bind_Param (Name => "user_id", Value => User);
      AWA.Workspaces.Models.List (Into.Members, Session, Query);
      Into.Count := ADO.Datasets.Get_Count (Session, Count_Query);
   end Load;

   --  ------------------------------
   --  Create the Member_List_Bean bean instance.
   --  ------------------------------
   function Create_Member_List_Bean (Module : in AWA.Workspaces.Modules.Workspace_Module_Access)
                                     return Util.Beans.Basic.Readonly_Bean_Access is
      Object  : constant Member_List_Bean_Access := new Member_List_Bean;
   begin
      Object.Module     := Module;
      Object.Members_Bean := Object.Members'Access;
      Object.Page_Size  := 20;
      Object.Page       := 1;
      Object.Count      := 0;
      return Object.all'Access;
   end Create_Member_List_Bean;

end AWA.Workspaces.Beans;
