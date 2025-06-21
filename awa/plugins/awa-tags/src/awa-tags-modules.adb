-----------------------------------------------------------------------
--  awa-tags-modules -- Module awa-tags
--  Copyright (C) 2013, 2016 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

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

   --  Add a tag on the database entity referenced by <tt>Id</tt> in the table identified
   --  by <tt>Entity_Type</tt>.
   procedure Add_Tag (Session     : in out ADO.Sessions.Master_Session;
                      Id          : in ADO.Identifier;
                      Entity_Type : in ADO.Entity_Type;
                      Tag         : in String);

   --  Remove the tag identified by <tt>Tag</tt> and associated with the database entity
   --  referenced by <tt>Id</tt> in the table identified by <tt>Entity_Type</tt>.
   procedure Remove_Tag (Session     : in out ADO.Sessions.Master_Session;
                         Id          : in ADO.Identifier;
                         Entity_Type : in ADO.Entity_Type;
                         Tag         : in String);

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

      --  Register the tag search bean.
      Register.Register (Plugin => Plugin,
                         Name   => "AWA.Tags.Beans.Tag_Search_Bean",
                         Handler => AWA.Tags.Beans.Create_Tag_Search_Bean'Access);

      --  Register the tag info list bean.
      Register.Register (Plugin => Plugin,
                         Name   => "AWA.Tags.Beans.Tag_Info_List_Bean",
                         Handler => AWA.Tags.Beans.Create_Tag_Info_List_Bean'Access);

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
      Kind   : ADO.Entity_Type;
   begin
      --  Check that the user has the permission on the given object.
      AWA.Permissions.Check (Permission => Security.Permissions.Get_Permission_Index (Permission),
                             Entity     => Id);

      Log.Info ("User {0} add tag {1} on {2}",
                ADO.Identifier'Image (User.Get_Id), Tag, Ident);

      Ctx.Start;
      Kind := ADO.Sessions.Entities.Find_Entity_Type (DB, Entity_Type);
      Add_Tag (DB, Id, Kind, Tag);
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
      Kind   : ADO.Entity_Type;
   begin
      --  Check that the user has the permission on the given object.
      AWA.Permissions.Check (Permission => Security.Permissions.Get_Permission_Index (Permission),
                             Entity     => Id);

      Log.Info ("User {0} removes tag {1} on {2}",
                ADO.Identifier'Image (User.Get_Id), Tag, Ident);

      Ctx.Start;
      Kind := ADO.Sessions.Entities.Find_Entity_Type (DB, Entity_Type);
      Remove_Tag (DB, Id, Kind, Tag);
      Ctx.Commit;
   end Remove_Tag;

   --  ------------------------------
   --  Remove the tags defined by the <tt>Deleted</tt> tag list and add the tags defined
   --  in the <tt>Added</tt> tag list.  The tags are associated with the database entity
   --  referenced by <tt>Id</tt> in the table identified by <tt>Entity_Type</tt>.
   --  The permission represented by <tt>Permission</tt> is checked to make sure the current user
   --  can remove or add the tag.
   --  ------------------------------
   procedure Update_Tags (Model       : in Tag_Module;
                          Id          : in ADO.Identifier;
                          Entity_Type : in String;
                          Permission  : in String;
                          Added       : in Util.Strings.Vectors.Vector;
                          Deleted     : in Util.Strings.Vectors.Vector) is
      pragma Unreferenced (Model);

      Ctx    : constant ASC.Service_Context_Access := ASC.Current;
      User   : constant AWA.Users.Models.User_Ref := Ctx.Get_User;
      Ident  : constant String := Entity_Type & ADO.Identifier'Image (Id);
      DB     : ADO.Sessions.Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
      Kind   : ADO.Entity_Type;
      Iter   : Util.Strings.Vectors.Cursor;
   begin
      --  Check that the user has the permission on the given object.
      AWA.Permissions.Check (Permission => Security.Permissions.Get_Permission_Index (Permission),
                             Entity     => Id);

      Ctx.Start;
      Kind := ADO.Sessions.Entities.Find_Entity_Type (DB, Entity_Type);

      --  Delete the tags that have been removed.
      Iter := Deleted.First;
      while Util.Strings.Vectors.Has_Element (Iter) loop
         declare
            Tag : constant String := Util.Strings.Vectors.Element (Iter);
         begin
            Log.Info ("User {0} removes tag {1} on {2}",
                      ADO.Identifier'Image (User.Get_Id), Tag, Ident);

            Remove_Tag (DB, Id, Kind, Tag);
         end;
         Util.Strings.Vectors.Next (Iter);
      end loop;

      --  And add the new ones.
      Iter := Added.First;
      while Util.Strings.Vectors.Has_Element (Iter) loop
         declare
            Tag : constant String := Util.Strings.Vectors.Element (Iter);
         begin
            Log.Info ("User {0} adds tag {1} on {2}",
                      ADO.Identifier'Image (User.Get_Id), Tag, Ident);

            Add_Tag (DB, Id, Kind, Tag);
         end;
         Util.Strings.Vectors.Next (Iter);
      end loop;
      Ctx.Commit;
   end Update_Tags;

   --  ------------------------------
   --  Add a tag on the database entity referenced by <tt>Id</tt> in the table identified
   --  by <tt>Entity_Type</tt>.
   --  ------------------------------
   procedure Add_Tag (Session     : in out ADO.Sessions.Master_Session;
                      Id          : in ADO.Identifier;
                      Entity_Type : in ADO.Entity_Type;
                      Tag         : in String) is
      Query    : ADO.Queries.Context;
      Tag_Info : AWA.Tags.Models.Tag_Ref;
      Tag_Link : AWA.Tags.Models.Tagged_Entity_Ref;
   begin
      Query.Set_Query (AWA.Tags.Models.Query_Check_Tag);

      declare
         Stmt : ADO.Statements.Query_Statement := Session.Create_Statement (Query);
      begin
         --  Build the query.
         Stmt.Bind_Param (Name => "entity_type", Value => Entity_Type);
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
            Tag_Link.Set_Entity_Type (Entity_Type);

            Tag_Info.Save (Session);
            Tag_Link.Save (Session);

         elsif Stmt.Is_Null (1) then
            --  The tag is defined but the entity is not linked with it.
            Tag_Info.Set_Id (Stmt.Get_Identifier (0));
            Tag_Link.Set_Tag (Tag_Info);
            Tag_Link.Set_For_Entity_Id (Id);
            Tag_Link.Set_Entity_Type (Entity_Type);
            Tag_Link.Save (Session);
         end if;
      end;
   end Add_Tag;

   --  ------------------------------
   --  Remove the tag identified by <tt>Tag</tt> and associated with the database entity
   --  referenced by <tt>Id</tt> in the table identified by <tt>Entity_Type</tt>.
   --  ------------------------------
   procedure Remove_Tag (Session     : in out ADO.Sessions.Master_Session;
                         Id          : in ADO.Identifier;
                         Entity_Type : in ADO.Entity_Type;
                         Tag         : in String) is
      Query    : ADO.SQL.Query;
      Tag_Link : AWA.Tags.Models.Tagged_Entity_Ref;
      Found    : Boolean;
   begin
      Query.Set_Join ("INNER JOIN awa_tag AS tag ON tag.id = o.tag_id");
      Query.Set_Filter ("tag.name = :tag AND o.for_entity_id = :entity_id "
                        & "AND o.entity_type = :entity_type");

      --  Build the query.
      Query.Bind_Param (Name => "entity_type", Value => Entity_Type);
      Query.Bind_Param (Name => "entity_id", Value => Id);
      Query.Bind_Param (Name => "tag", Value => Tag);

      Tag_Link.Find (Session, Query, Found);
      if Found then
         Tag_Link.Delete (Session);
      end if;
   end Remove_Tag;

   --  ------------------------------
   --  Find the tag identifier associated with the given tag.
   --  Return NO_IDENTIFIER if there is no such tag.
   --  ------------------------------
   procedure Find_Tag_Id (Session : in out ADO.Sessions.Session'Class;
                          Tag     : in String;
                          Result  : out ADO.Identifier) is
   begin
      if Tag'Length = 0 then
         Result := ADO.NO_IDENTIFIER;
      else
         declare
            Query    : ADO.SQL.Query;
            Found    : Boolean;
            Tag_Info : AWA.Tags.Models.Tag_Ref;
         begin
            Query.Set_Filter ("o.name = ?");
            Query.Bind_Param (1, Tag);
            Tag_Info.Find (Session => Session,
                           Query   => Query,
                           Found   => Found);
            if not Found then
               Result := ADO.NO_IDENTIFIER;
            else
               Result := Tag_Info.Get_Id;
            end if;
         end;
      end if;
   end Find_Tag_Id;

end AWA.Tags.Modules;
