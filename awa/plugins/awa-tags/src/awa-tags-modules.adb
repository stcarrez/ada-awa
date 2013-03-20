-----------------------------------------------------------------------
--  awa-tags-modules -- Module awa-tags
--  Copyright (C) 2013 Stephane Carrez
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

with ADO.Sessions;
with ADO.Sessions.Entities;
with ADO.Statements;
with ADO.Queries;
with ADO.SQL;

with Security.Permissions;

with AWA.Permissions;
with AWA.Services.Contexts;
with AWA.Modules.Get;
with AWA.Modules.Beans;
with AWA.Users.Models;
with AWA.Tags.Models;
with AWA.Tags.Beans;
with AWA.Tags.Components;

with Util.Log.Loggers;
package body AWA.Tags.Modules is

   package ASC renames AWA.Services.Contexts;

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("AWA.Tags.Module");

   package Register is new AWA.Modules.Beans (Module        => Tag_Module,
                                              Module_Access => Tag_Module_Access);

   --  ------------------------------
   --  Initialize the tags module.
   --  ------------------------------
   overriding
   procedure Initialize (Plugin : in out Tag_Module;
                         App    : in AWA.Modules.Application_Access;
                         Props  : in ASF.Applications.Config) is
   begin
      Log.Info ("Initializing the awa-tags module");

      App.Add_Components (AWA.Tags.Components.Definition);

      --  Register the tag list bean.
      Register.Register (Plugin => Plugin,
                         Name   => "AWA.Tags.Beans.Tag_List_Bean",
                         Handler => AWA.Tags.Beans.Create_Tag_List_Bean'Access);

      AWA.Modules.Module (Plugin).Initialize (App, Props);

      --  Add here the creation of manager instances.
   end Initialize;

   --  ------------------------------
   --  Get the tags module.
   --  ------------------------------
   function Get_Tag_Module return Tag_Module_Access is
      function Get is new AWA.Modules.Get (Tag_Module, Tag_Module_Access, NAME);
   begin
      return Get;
   end Get_Tag_Module;

   --  ------------------------------
   --  Add a tag on the database entity referenced by <tt>Id</tt> in the table identified
   --  by <tt>Entity_Type</tt>.  The permission represented by <tt>Permission</tt> is checked
   --  to make sure the current user can add the tag.  If the permission is granted, the
   --  tag represented by <tt>Tag</tt> is associated with the said database entity.
   --  ------------------------------
   procedure Add_Tag (Model       : in Tag_Module;
                      Id          : in ADO.Identifier;
                      Entity_Type : in String;
                      Permission  : in String;
                      Tag         : in String) is
      pragma Unreferenced (Model);

      Ctx    : constant ASC.Service_Context_Access := ASC.Current;
      User   : constant AWA.Users.Models.User_Ref := Ctx.Get_User;
      Ident  : constant String := Entity_Type & ADO.Identifier'Image (Id);
      DB     : ADO.Sessions.Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
      Query  : ADO.Queries.Context;
   begin
      --  Check that the user has the permission on the given object.
      AWA.Permissions.Check (Permission => Security.Permissions.Get_Permission_Index (Permission),
                             Entity     => Id);

      Log.Info ("User {0} add tag {1} on {2}",
                ADO.Identifier'Image (User.Get_Id), Tag, Ident);

      Query.Set_Query (AWA.Tags.Models.Query_Check_Tag);
      Ctx.Start;
      declare
         Kind     : ADO.Entity_Type := ADO.Sessions.Entities.Find_Entity_Type (DB, Entity_Type);
         Stmt     : ADO.Statements.Query_Statement := DB.Create_Statement (Query);
         Tag_Info : AWA.Tags.Models.Tag_Ref;
         Tag_Link : AWA.Tags.Models.Tagged_Entity_Ref;
      begin
         --  Build the query.
         Stmt.Bind_Param (Name => "entity_type", Value => Kind);
         Stmt.Bind_Param (Name => "entity_id", Value => Id);
         Stmt.Bind_Param (Name => "tag", Value => Tag);

         --  Run the query.
         Stmt.Execute;

         if not Stmt.Has_Elements then
            --  The tag is not defined in the database.
            --  Create it and link it to the entity.
            Tag_Info.Set_Name (Tag);
            Tag_Link.Set_Tag (Tag_Info);
            Tag_Link.Set_For_Entity_Id (Id);
            Tag_Link.Set_Entity_Type (Kind);

            Tag_Info.Save (DB);
            Tag_Link.Save (DB);

         elsif Stmt.Is_Null (1) then
            --  The tag is defined but the entity is not linked with it.
            Tag_Info.Set_Id (Stmt.Get_Identifier (0));
            Tag_Link.Set_Tag (Tag_Info);
            Tag_Link.Set_For_Entity_Id (Id);
            Tag_Link.Set_Entity_Type (Kind);
            Tag_Link.Save (DB);
         end if;
      end;
      Ctx.Commit;
   end Add_Tag;

   --  ------------------------------
   --  Remove the tag identified by <tt>Tag</tt> and associated with the database entity
   --  referenced by <tt>Id</tt> in the table identified by <tt>Entity_Type</tt>.
   --  The permission represented by <tt>Permission</tt> is checked to make sure the current user
   --  can remove the tag.
   --  ------------------------------
   procedure Remove_Tag (Model       : in Tag_Module;
                         Id          : in ADO.Identifier;
                         Entity_Type : in String;
                         Permission  : in String;
                         Tag         : in String) is
      pragma Unreferenced (Model);

      Ctx    : constant ASC.Service_Context_Access := ASC.Current;
      User   : constant AWA.Users.Models.User_Ref := Ctx.Get_User;
      Ident  : constant String := Entity_Type & ADO.Identifier'Image (Id);
      DB     : ADO.Sessions.Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
      Query  : ADO.SQL.Query;
   begin
      --  Check that the user has the permission on the given object.
      AWA.Permissions.Check (Permission => Security.Permissions.Get_Permission_Index (Permission),
                             Entity     => Id);

      Log.Info ("User {0} removes tag {1} on {2}",
                ADO.Identifier'Image (User.Get_Id), Tag, Ident);

      Query.Set_Join ("INNER JOIN awa_tag AS tag ON tag.id = o.tag_id");
      Query.Set_Filter ("tag.name = :tag AND o.for_entity_id = :entity_id "
                        & "AND o.entity_type = :entity_type");

      Ctx.Start;
      declare
         Kind     : ADO.Entity_Type := ADO.Sessions.Entities.Find_Entity_Type (DB, Entity_Type);
         Tag_Link : AWA.Tags.Models.Tagged_Entity_Ref;
         Found    : Boolean;
      begin
         --  Build the query.
         Query.Bind_Param (Name => "entity_type", Value => Kind);
         Query.Bind_Param (Name => "entity_id", Value => Id);
         Query.Bind_Param (Name => "tag", Value => Tag);

         Tag_Link.Find (DB, Query, Found);
         if Found then
            Tag_Link.Delete (DB);
         end if;
      end;
      Ctx.Commit;
   end Remove_Tag;

end AWA.Tags.Modules;
