-----------------------------------------------------------------------
--  awa-permissions-services -- Permissions controller
--  Copyright (C) 2011, 2012, 2013, 2017 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Beans.Objects;

with EL.Functions;

with ADO;
with ADO.Sessions;
with ADO.Objects;

with Security.Policies;
with Security.Contexts;
with Security.Policies.Roles;

with AWA.Applications;
with AWA.Services.Contexts;
package AWA.Permissions.Services is

   package ASC renames AWA.Services.Contexts;

   --  Register the security EL functions in the EL mapper.
   procedure Set_Functions (Mapper : in out EL.Functions.Function_Mapper'Class);

   type Permission_Manager is new Security.Policies.Policy_Manager with private;
   type Permission_Manager_Access is access all Permission_Manager'Class;

   --  Get the permission manager associated with the security context.
   --  Returns null if there is none.
   function Get_Permission_Manager (Context : in Security.Contexts.Security_Context'Class)
                                    return Permission_Manager_Access;

   --  Get the permission manager associated with the security context.
   --  Returns null if there is none.
   function Get_Permission_Manager (Context : in ASC.Service_Context_Access)
                                    return Permission_Manager_Access;

   --  Get the application instance.
   function Get_Application (Manager : in Permission_Manager)
                             return AWA.Applications.Application_Access;

   --  Set the application instance.
   procedure Set_Application (Manager : in out Permission_Manager;
                              App : in AWA.Applications.Application_Access);

   --  Initialize the permissions.
   procedure Start (Manager : in out Permission_Manager);

   --  Add a permission for the current user to access the entity identified by
   --  <b>Entity</b> and <b>Kind</b> in the <b>Workspace</b>.
   procedure Add_Permission (Manager    : in Permission_Manager;
                             Entity     : in ADO.Identifier;
                             Kind       : in ADO.Entity_Type;
                             Workspace  : in ADO.Identifier;
                             Permission : in Security.Permissions.Permission_Index);

   --  Check that the current user has the specified permission.
   --  Raise NO_PERMISSION exception if the user does not have the permission.
   procedure Check_Permission (Manager    : in Permission_Manager;
                               Entity     : in ADO.Identifier;
                               Kind       : in ADO.Entity_Type;
                               Permission : in Permission_Type);

   --  Get the role names that grant the given permission.
   function Get_Role_Names (Manager : in Permission_Manager;
                            Permission : in Security.Permissions.Permission_Index)
                            return Security.Policies.Roles.Role_Name_Array;

   --  Add a permission for the user <b>User</b> to access the entity identified by
   --  <b>Entity</b> which is of type <b>Kind</b>.
   procedure Add_Permission (Manager    : in Permission_Manager;
                             Session    : in out ADO.Sessions.Master_Session;
                             User       : in ADO.Identifier;
                             Entity     : in ADO.Identifier;
                             Kind       : in ADO.Entity_Type;
                             Workspace  : in ADO.Identifier;
                             Permission : in Security.Permissions.Permission_Index);

   --  Add a permission for the user <b>User</b> to access the entity identified by
   --  <b>Entity</b>.
   procedure Add_Permission (Manager    : in Permission_Manager;
                             Session    : in out ADO.Sessions.Master_Session;
                             User       : in ADO.Identifier;
                             Entity     : in ADO.Objects.Object_Ref'Class;
                             Workspace  : in ADO.Identifier;
                             Permission : in Security.Permissions.Permission_Index);

   --  Create a permission manager for the given application.
   function Create_Permission_Manager (App : in AWA.Applications.Application_Access)
                                       return Security.Policies.Policy_Manager_Access;

   --  Check if the permission with the name <tt>Name</tt> is granted for the current user.
   --  If the <tt>Entity</tt> is defined, an <tt>Entity_Permission</tt> is created and verified.
   --  Returns True if the user is granted the given permission.
   function Has_Permission (Name   : in Util.Beans.Objects.Object;
                            Entity : in Util.Beans.Objects.Object)
                            return Util.Beans.Objects.Object;

   --  Delete all the permissions for a user and on the given workspace.
   procedure Delete_Permissions (Session   : in out ADO.Sessions.Master_Session;
                                 User      : in ADO.Identifier;
                                 Workspace : in ADO.Identifier);

private

   type Permission_Array is array (Security.Permissions.Permission_Index) of ADO.Identifier;

   type Permission_Manager is new Security.Policies.Policy_Manager with record
      App   : AWA.Applications.Application_Access;
      Roles : Security.Policies.Roles.Role_Policy_Access;

      --  Mapping between the application permission index and the database permission identifier.
      Map   : Permission_Array := (others => 0);
   end record;

end AWA.Permissions.Services;
