-----------------------------------------------------------------------
--  awa-permissions-configs -- Permission configuration
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

with Security.Permissions;

package AWA.Permissions.Configs is

   --  ------------------------------
   --  Security Controller
   --  ------------------------------

   type Controller_Config is record
      Name    : Util.Beans.Objects.Object;
      SQL     : Util.Beans.Objects.Object;
      Entity  : ADO.Entity_Type := 0;
      Count   : Natural := 0;
      Manager : Security.Permissions.Permission_Manager_Access;
      Session : ADO.Sessions.Session;
   end record;

   type Config_Fields is (FIELD_NAME, FIELD_ENTITY_TYPE, FIELD_ENTITY_PERMISSION, FIELD_SQL);

   type Controller_Config_Access is access all Controller_Config;

   procedure Set_Member (Into  : in out Controller_Config;
                         Field : in Config_Fields;
                         Value : in Util.Beans.Objects.Object);

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

end AWA.Permissions.Configs;
