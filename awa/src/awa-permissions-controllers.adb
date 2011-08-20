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

with ADO.Schemas.Entities;
with ADO.Sessions.Entities;

with Util.Serialize.Mappers.Record_Mapper;
package body AWA.Permissions.Controllers is

   --  ------------------------------
   --  Security Controller
   --  ------------------------------

   --  Returns true if the user associated with the security context <b>Context</b> has
   --  one of the role defined in the <b>Handler</b>.
   function Has_Permission (Handler : in Entity_Controller;
                            Context : in Security.Contexts.Security_Context'Class)
                            return Boolean is
   begin
      return False;
   end Has_Permission;

   type Config_Fields is (FIELD_NAME, FIELD_ENTITY_TYPE, FIELD_ENTITY_PERMISSION, FIELD_SQL);

   type Controller_Config_Access is access all Controller_Config;

   procedure Set_Member (Into  : in out Controller_Config;
                         Field : in Config_Fields;
                         Value : in Util.Beans.Objects.Object);

   --  ------------------------------
   --  Called while parsing the XML policy file when the <name>, <entity-type>, <sql> and
   --  <entity-permission> XML entities are found.  Create the new permission when the complete
   --  permission definition has been parsed and save the permission in the security manager.
   --  ------------------------------
   procedure Set_Member (Into  : in out Controller_Config;
                         Field : in Config_Fields;
                         Value : in Util.Beans.Objects.Object) is
   begin
      case Field is
         when FIELD_NAME =>
            Into.Name := Value;

         when FIELD_SQL =>
            Into.SQL := Value;

         when FIELD_ENTITY_TYPE =>
            declare
               Name : constant String := Util.Beans.Objects.To_String (Value);
            begin
               Into.Entity := ADO.Sessions.Entities.Find_Entity_Type (Into.Session, Name);

            exception
               when ADO.Schemas.Entities.No_Entity_Type =>
                  raise Util.Serialize.Mappers.Field_Error with "Invalid entity type: " & Name;
            end;

         when FIELD_ENTITY_PERMISSION =>
            if Into.Count = 0 then
               raise Util.Serialize.Mappers.Field_Error with "Missing at least one role";
            end if;
            declare
               Name : constant String := Util.Beans.Objects.To_String (Into.Name);
               Perm : constant Entity_Controller_Access
                 := new Entity_Controller '(SQL    => Util.Beans.Objects.To_Unbounded_String (Into.SQL),
                                            Entity => Into.Entity);
            begin
               Into.Manager.Add_Permission (Name, Perm.all'Access);
               Into.Count := 0;
            end;

      end case;
   end Set_Member;

   package Config_Mapper is
     new Util.Serialize.Mappers.Record_Mapper (Element_Type        => Controller_Config,
                                               Element_Type_Access => Controller_Config_Access,
                                               Fields              => Config_Fields,
                                               Set_Member          => Set_Member);

   Mapper : aliased Config_Mapper.Mapper;

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

   package body Reader_Config is
   begin
      Reader.Add_Mapping ("policy-rules", Mapper'Access);
      Config.Manager := Manager;
      Config_Mapper.Set_Context (Reader, Config'Unchecked_Access);
   end Reader_Config;

begin
   Mapper.Add_Mapping ("entity-permission", FIELD_ENTITY_PERMISSION);
   Mapper.Add_Mapping ("entity-permission/name", FIELD_NAME);
   Mapper.Add_Mapping ("entity-permission/entity-type", FIELD_ENTITY_TYPE);
   Mapper.Add_Mapping ("entity-permission/sql", FIELD_SQL);
end AWA.Permissions.Controllers;
