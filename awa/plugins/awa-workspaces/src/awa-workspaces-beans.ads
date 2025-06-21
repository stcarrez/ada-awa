-----------------------------------------------------------------------
--  awa-workspaces-beans -- Beans for module workspaces
--  Copyright (C) 2011, 2012, 2017 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.Strings.Unbounded;

with Util.Beans.Basic;
with Util.Beans.Objects;
with Util.Beans.Methods;

with AWA.Events;
with AWA.Users.Models;
with AWA.Workspaces.Models;
with AWA.Workspaces.Modules;
package AWA.Workspaces.Beans is

   type Member_Bean is new AWA.Workspaces.Models.Member_Bean with record
      Module  : AWA.Workspaces.Modules.Workspace_Module_Access := null;
   end record;
   type Member_Bean_Access is access all Member_Bean'Class;

   --  Get the value identified by the name.
   overriding
   function Get_Value (From : in Member_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the value identified by the name
   overriding
   procedure Set_Value (Item  : in out Member_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   overriding
   procedure Load (Bean : in out Member_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   overriding
   procedure Delete (Bean : in out Member_Bean;
                     Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   --  Create the Member_Bean bean instance.
   function Create_Member_Bean (Module : in AWA.Workspaces.Modules.Workspace_Module_Access)
                                return Util.Beans.Basic.Readonly_Bean_Access;

   type Invitation_Bean is new AWA.Workspaces.Models.Invitation_Bean with record
      Module  : AWA.Workspaces.Modules.Workspace_Module_Access := null;
      Inviter : AWA.Users.Models.User_Ref;
   end record;
   type Invitation_Bean_Access is access all Invitation_Bean'Class;

   --  Get the value identified by the name.
   overriding
   function Get_Value (From : in Invitation_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   overriding
   procedure Load (Bean : in out Invitation_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   overriding
   procedure Accept_Invitation (Bean    : in out Invitation_Bean;
                                Event   : in AWA.Events.Module_Event'Class);

   overriding
   procedure Send (Bean    : in out Invitation_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   --  Create the Invitation_Bean bean instance.
   function Create_Invitation_Bean (Module : in AWA.Workspaces.Modules.Workspace_Module_Access)
                                     return Util.Beans.Basic.Readonly_Bean_Access;

   type Workspaces_Bean is new Util.Beans.Basic.Bean
     and Util.Beans.Methods.Method_Bean with record
      Module : AWA.Workspaces.Modules.Workspace_Module_Access := null;
      Count  : Natural := 0;
   end record;
   type Workspaces_Bean_Access is access all Workspaces_Bean'Class;

   --  Get the value identified by the name.
   overriding
   function Get_Value (From : in Workspaces_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the value identified by the name.
   overriding
   procedure Set_Value (From  : in out Workspaces_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   --  This bean provides some methods that can be used in a Method_Expression
   overriding
   function Get_Method_Bindings (From : in Workspaces_Bean)
                                 return Util.Beans.Methods.Method_Binding_Array_Access;

   --  Event action called to create the workspace when the given event is posted.
   procedure Create (Bean  : in out Workspaces_Bean;
                     Event : in AWA.Events.Module_Event'Class);

   --  Create the Workspaces_Bean bean instance.
   function Create_Workspaces_Bean (Module : in AWA.Workspaces.Modules.Workspace_Module_Access)
      return Util.Beans.Basic.Readonly_Bean_Access;

   type Member_List_Bean is new AWA.Workspaces.Models.Member_List_Bean with record
      Module       : AWA.Workspaces.Modules.Workspace_Module_Access;

      --  The list of workspace members.
      Members      : aliased AWA.Workspaces.Models.Member_Info_List_Bean;
      Members_Bean : Util.Beans.Basic.Readonly_Bean_Access;
   end record;
   type Member_List_Bean_Access is access all Member_List_Bean'Class;

   --  Get the value identified by the name.
   overriding
   function Get_Value (From : in Member_List_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Load the list of members.
   overriding
   procedure Load (Into    : in out Member_List_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   --  Create the Member_List_Bean bean instance.
   function Create_Member_List_Bean (Module : in AWA.Workspaces.Modules.Workspace_Module_Access)
                                     return Util.Beans.Basic.Readonly_Bean_Access;

end AWA.Workspaces.Beans;
