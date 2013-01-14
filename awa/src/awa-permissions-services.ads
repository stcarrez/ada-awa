-----------------------------------------------------------------------
--  awa-permissions-services -- Permissions controller
--  Copyright (C) 2011, 2012, 2013 Stephane Carrez
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

with AWA.Applications;

with Util.Beans.Objects;

with ADO;
with ADO.Sessions;
with ADO.Objects;

with Security.Policies;
with Security.Contexts;
package AWA.Permissions.Services is

   type Permission_Manager is new Security.Policies.Policy_Manager with private;
   type Permission_Manager_Access is access all Permission_Manager'Class;

   --  Get the permission manager associated with the security context.
   --  Returns null if there is none.
   function Get_Permission_Manager (Context : in Security.Contexts.Security_Context'Class)
                                    return Permission_Manager_Access;

   --  Get the application instance.
   function Get_Application (Manager : in Permission_Manager)
                             return AWA.Applications.Application_Access;

   --  Set the application instance.
   procedure Set_Application (Manager : in out Permission_Manager;
                              App : in AWA.Applications.Application_Access);

   --  Add a permission for the current user to access the entity identified by
   --  <b>Entity</b> and <b>Kind</b>.
   procedure Add_Permission (Manager    : in Permission_Manager;
                             Entity     : in ADO.Identifier;
                             Kind       : in ADO.Entity_Type;
                             Permission : in Permission_Type);

   --  Check that the current user has the specified permission.
   --  Raise NO_PERMISSION exception if the user does not have the permission.
   procedure Check_Permission (Manager    : in Permission_Manager;
                               Entity     : in ADO.Identifier;
                               Kind       : in ADO.Entity_Type;
                               Permission : in Permission_Type);

   --  Add a permission for the user <b>User</b> to access the entity identified by
   --  <b>Entity</b> which is of type <b>Kind</b>.
   procedure Add_Permission (Session    : in out ADO.Sessions.Master_Session;
                             User       : in ADO.Identifier;
                             Entity     : in ADO.Identifier;
                             Kind       : in ADO.Entity_Type;
                             Permission : in Permission_Type := READ);

   --  Add a permission for the user <b>User</b> to access the entity identified by
   --  <b>Entity</b>.
   procedure Add_Permission (Session    : in out ADO.Sessions.Master_Session;
                             User       : in ADO.Identifier;
                             Entity     : in ADO.Objects.Object_Ref'Class;
                             Permission : in Permission_Type := READ);

   --  Create a permission manager for the given application.
   function Create_Permission_Manager (App : in AWA.Applications.Application_Access)
                                       return Security.Policies.Policy_Manager_Access;

   --  Check if the permission with the name <tt>Name</tt> is granted for the current user.
   --  If the <tt>Entity</tt> is defined, an <tt>Entity_Permission</tt> is created and verified.
   --  Returns True if the user is granted the given permission.
   function Has_Permission (Name   : in Util.Beans.Objects.Object;
                            Entity : in Util.Beans.Objects.Object)
                            return Util.Beans.Objects.Object;

private

   type Permission_Manager is new Security.Policies.Policy_Manager with record
      App : AWA.Applications.Application_Access := null;
   end record;

end AWA.Permissions.Services;
