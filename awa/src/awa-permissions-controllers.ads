-----------------------------------------------------------------------
--  awa-permissions-controllers -- Permission controllers
--  Copyright (C) 2011, 2012, 2013, 2014, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Security.Contexts;
with Security.Controllers;

package AWA.Permissions.Controllers is

   --  ------------------------------
   --  Security Controller
   --  ------------------------------
   --  The <b>Entity_Controller</b> implements an entity based permission check.
   --  The controller is configured through an XML description.  It uses an SQL statement
   --  to verify that a permission is granted.
   --
   --  The SQL query can use the following query parameters:
   --
   --  <dl>
   --    <dt>entity_type</dt>
   --    <dd>The entity type identifier defined by the entity permission</dd>
   --    <dt>entity_id</dt>
   --    <dd>The entity identifier which is associated with an <b>ACL</b> entry to check</dd>
   --    <dt>user_id</dt>
   --    <dd>The user identifier</dd>
   --  </dl>
   type Entity_Controller (Len : Positive;
                           Use_Entity_Id : Boolean) is
   limited new Security.Controllers.Controller with record
      Entities : Entity_Type_Array;
      SQL      : String (1 .. Len);
   end record;
   type Entity_Controller_Access is access all Entity_Controller'Class;

   --  Returns true if the user associated with the security context <b>Context</b> has
   --  the permission to access a database entity.  The security context contains some
   --  information about the entity to check and the permission controller will use an
   --  SQL statement to verify the permission.
   overriding
   function Has_Permission (Handler : in Entity_Controller;
                            Context : in Security.Contexts.Security_Context'Class;
                            Permission : in Security.Permissions.Permission'Class)
                            return Boolean;

end AWA.Permissions.Controllers;
