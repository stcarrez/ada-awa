-----------------------------------------------------------------------
--  awa-tags-modules -- Module awa-tags
--  Copyright (C) 2013, 2016 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with ASF.Applications;

with ADO;
with ADO.Sessions;

with Util.Strings.Vectors;

with AWA.Modules;

--  == Integration ==
--  The <tt>Tag_Module</tt> manages the tags associated with entities.  It provides operations
--  that are used by the tag beans together with the <tt>awa:tagList</tt> and
--  <tt>awa:tagCloud</tt> components to manage the tags.
--  An instance of the <tt>Tag_Module</tt> must be declared and registered in the AWA application.
--  The module instance can be defined as follows:
--
--    type Application is new AWA.Applications.Application with record
--       Tag_Module : aliased AWA.Tags.Modules.Tag_Module;
--    end record;
--
--  And registered in the `Initialize_Modules` procedure by using:
--
--    Register (App    => App.Self.all'Access,
--              Name   => AWA.Tags.Modules.NAME,
--              URI    => "tags",
--              Module => App.Tag_Module'Access);
package AWA.Tags.Modules is

   --  The name under which the module is registered.
   NAME : constant String := "tags";

   --  ------------------------------
   --  Module awa-tags
   --  ------------------------------
   type Tag_Module is new AWA.Modules.Module with private;
   type Tag_Module_Access is access all Tag_Module'Class;

   --  Initialize the tags module.
   overriding
   procedure Initialize (Plugin : in out Tag_Module;
                         App    : in AWA.Modules.Application_Access;
                         Props  : in ASF.Applications.Config);

   --  Get the tags module.
   function Get_Tag_Module return Tag_Module_Access;

   --  Add a tag on the database entity referenced by <tt>Id</tt> in the table identified
   --  by <tt>Entity_Type</tt>.  The permission represented by <tt>Permission</tt> is checked
   --  to make sure the current user can add the tag.  If the permission is granted, the
   --  tag represented by <tt>Tag</tt> is associated with the said database entity.
   procedure Add_Tag (Model       : in Tag_Module;
                      Id          : in ADO.Identifier;
                      Entity_Type : in String;
                      Permission  : in String;
                      Tag         : in String);

   --  Remove the tag identified by <tt>Tag</tt> and associated with the database entity
   --  referenced by <tt>Id</tt> in the table identified by <tt>Entity_Type</tt>.
   --  The permission represented by <tt>Permission</tt> is checked to make sure the current user
   --  can remove the tag.
   procedure Remove_Tag (Model       : in Tag_Module;
                         Id          : in ADO.Identifier;
                         Entity_Type : in String;
                         Permission  : in String;
                         Tag         : in String);

   --  Remove the tags defined by the <tt>Deleted</tt> tag list and add the tags defined
   --  in the <tt>Added</tt> tag list.  The tags are associated with the database entity
   --  referenced by <tt>Id</tt> in the table identified by <tt>Entity_Type</tt>.
   --  The permission represented by <tt>Permission</tt> is checked to make sure the current user
   --  can remove or add the tag.
   procedure Update_Tags (Model       : in Tag_Module;
                          Id          : in ADO.Identifier;
                          Entity_Type : in String;
                          Permission  : in String;
                          Added       : in Util.Strings.Vectors.Vector;
                          Deleted     : in Util.Strings.Vectors.Vector);

   --  Find the tag identifier associated with the given tag.
   --  Return NO_IDENTIFIER if there is no such tag.
   procedure Find_Tag_Id (Session : in out ADO.Sessions.Session'Class;
                          Tag     : in String;
                          Result  : out ADO.Identifier);

private

   type Tag_Module is new AWA.Modules.Module with null record;

end AWA.Tags.Modules;
