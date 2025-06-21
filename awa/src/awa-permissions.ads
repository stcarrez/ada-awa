-----------------------------------------------------------------------
--  awa-permissions -- Permissions module
--  Copyright (C) 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2021 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Security.Permissions;
with Security.Policies;

with Util.Serialize.Mappers;
private with Util.Beans.Objects;

with ADO;
with ADO.Objects;
private with ADO.Sessions;

--  == AWA Permissions ==
--  The *AWA.Permissions* framework defines and controls the permissions used by an application
--  to verify and grant access to the data and application service.  The framework provides a
--  set of services and API that helps an application in enforcing its specific permissions.
--  Permissions are verified by a permission controller which uses the service context to
--  have information about the user and other context.  The framework allows to use different
--  kinds of permission controllers.  The `Entity_Controller` is the default permission
--  controller which uses the database and an XML configuration to verify a permission.
--
--  === Declaration ===
--  To be used in the application, the first step is to declare the permission.
--  This is a static definition of the permission that will be used to ask to verify the
--  permission.  The permission is given a unique name that will be used in configuration files:
--
--    with Security.Permissions;
--    ...
--    package ACL_Create_Post is new Security.Permissions.Definition ("blog-create-post");
--
--  === Checking for a permission ===
--  A permission can be checked in Ada as well as in the presentation pages.
--  This is done by using the `Check` procedure and the permission definition.  This operation
--  acts as a barrier: it does not return anything but returns normally if the permission is
--  granted.  If the permission is denied, it raises the `NO_PERMISSION` exception.
--
--  Several `Check` operation exists.  Some require no argument and some others need a context
--  such as some entity identifier to perform the check.
--
--    with AWA.Permissions;
--    ...
--    AWA.Permissions.Check (Permission => ACL_Create_Post.Permission,
--                           Entity     => Blog_Id);
--
--  === Configuring a permission ===
--  The *AWA.Permissions* framework supports a simple permission model
--  The application configuration file must provide some information to help in checking the
--  permission.  The permission name is referenced by the `name` XML entity.  The `entity-type`
--  refers to the database entity (ie, the table) that the permission concerns.
--  The `sql` XML entity represents the SQL statement that must be used to verify the permission.
--
--    <entity-permission>
--      <name>blog-create-post</name>
--      <entity-type>blog</entity-type>
--      <description>Permission to create a new post.</description>
--      <sql>
--        SELECT acl.id FROM acl
--        WHERE acl.entity_type = :entity_type
--        AND acl.user_id = :user_id
--        AND acl.entity_id = :entity_id
--      </sql>
--    </entity-permission>
--
--  === Adding a permission ===
--  Adding a permission means to create an `ACL` database record that links a given database
--  entity to the user.  This is done easily with the `Add_Permission` procedure:
--
--    with AWA.Permissions.Services;
--    ...
--    AWA.Permissions.Services.Add_Permission (Session => DB,
--                                             User    => User,
--                                             Entity  => Blog);
--
--  === Data Model ===
--  [images/awa_permissions_model.png]
--
--  === Queries ===
--  @include permissions.xml
--
package AWA.Permissions is

   NAME : constant String := "Entity-Policy";

   --  Exception raised by the <b>Check</b> procedure if the user does not have
   --  the permission.
   NO_PERMISSION : exception;

   --  Maximum number of entity types that can be defined in the XML entity-permission.
   --  Most permission need only one entity type.  Keep that number small.
   MAX_ENTITY_TYPES : constant Positive := 5;

   type Entity_Type_Array is array (1 .. MAX_ENTITY_TYPES) of ADO.Entity_Type;

   type Permission_Type is (READ, WRITE);

   type Entity_Permission is new Security.Permissions.Permission with private;

   --  Verify that the permission represented by <b>Permission</b> is granted.
   --
   procedure Check (Permission : in Security.Permissions.Permission_Index);

   --  Verify that the permission represented by <b>Permission</b> is granted to access the
   --  database entity represented by <b>Entity</b>.
   procedure Check (Permission : in Security.Permissions.Permission_Index;
                    Entity     : in ADO.Identifier);

   --  Verify that the permission represented by <b>Permission</b> is granted to access the
   --  database entity represented by <b>Entity</b>.
   procedure Check (Permission : in Security.Permissions.Permission_Index;
                    Entity     : in ADO.Objects.Object_Ref'Class);

private

   type Entity_Permission is new Security.Permissions.Permission with record
      Entity : ADO.Identifier;
   end record;

   type Entity_Policy is tagged;
   type Entity_Policy_Access is access all Entity_Policy'Class;

   type Controller_Config is record
      Name     : Util.Beans.Objects.Object;
      SQL      : Util.Beans.Objects.Object;
      Grant    : Util.Beans.Objects.Object;
      Entity   : ADO.Entity_Type := 0;
      Entities : Entity_Type_Array := (others => ADO.NO_ENTITY_TYPE);
      Count    : Natural := 0;
      Manager  : Entity_Policy_Access;
      Session  : ADO.Sessions.Session;
   end record;

   type Entity_Policy is new Security.Policies.Policy with record
      Config : aliased Controller_Config;
   end record;

   --  Get the policy name.
   overriding
   function Get_Name (From : in Entity_Policy) return String;

   --  Setup the XML parser to read the <b>policy</b> description.
   overriding
   procedure Prepare_Config (Policy : in out Entity_Policy;
                             Mapper : in out Util.Serialize.Mappers.Processing);

end AWA.Permissions;
