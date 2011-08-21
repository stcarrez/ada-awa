-----------------------------------------------------------------------
--  awa-permissions-controllers -- Permission controllers
--  Copyright (C) 2011 Stephane Carrez
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

with Util.Beans.Objects;
with Util.Serialize.IO.XML;

with ADO.Sessions;

with Security.Contexts;
with Security.Controllers;
with Security.Permissions;

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
   type Entity_Controller (Len : Positive) is
   limited new Security.Controllers.Controller with record
      SQL    : String (1 .. Len);
      Entity : ADO.Entity_Type;
   end record;
   type Entity_Controller_Access is access all Entity_Controller'Class;

   --  Returns true if the user associated with the security context <b>Context</b> has
   --  the permission to access a database entity.  The security context contains some
   --  information about the entity to check and the permission controller will use an
   --  SQL statement to verify the permission.
   function Has_Permission (Handler : in Entity_Controller;
                            Context : in Security.Contexts.Security_Context'Class)
                            return Boolean;

   type Controller_Config is record
      Name    : Util.Beans.Objects.Object;
      SQL     : Util.Beans.Objects.Object;
      Entity  : ADO.Entity_Type;
      Count   : Natural := 0;
      Manager : Security.Permissions.Permission_Manager_Access;
      Session : ADO.Sessions.Session;
   end record;

   --  Setup the XML parser to read the <b>entity-permission</b> description.  For example:
   --
   --  <entity-permission>
   --     <name>create-workspace</name>
   --     <entity-type>WORKSPACE</entity-type>
   --     <sql>select acl.id from acl where ...</sql>
   --  </entity-permission>
   --
   --  This defines a permission <b>create-workspace</b> that will be granted if the
   --  SQL statement returns a non empty list.
   generic
      Reader  : in out Util.Serialize.IO.XML.Parser;
      Manager : in Security.Permissions.Permission_Manager_Access;
   package Reader_Config is
      Config : aliased Controller_Config;
   end Reader_Config;

end AWA.Permissions.Controllers;
