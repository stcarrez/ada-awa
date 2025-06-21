-----------------------------------------------------------------------
--  awa-tags-beans -- Beans for the tags module
--  Copyright (C) 2013 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Unchecked_Deallocation;

with ADO.Queries;
with ADO.Statements;
with ADO.Sessions.Entities;

package body AWA.Tags.Beans is

   --  ------------------------------
   --  Compare two tags on their count and name.
   --  ------------------------------
   function "<" (Left, Right : in AWA.Tags.Models.Tag_Info) return Boolean is
      use type Ada.Strings.Unbounded.Unbounded_String;
   begin
      if Left.Count = Right.Count then
         return Left.Tag < Right.Tag;
      else
         return Left.Count > Right.Count;
      end if;
   end "<";

   --  ------------------------------
   --  Set the value identified by the name.
   --  If the name cannot be found, the method should raise the No_Value
   --  exception.
   --  ------------------------------
   overriding
   procedure Set_Value (From  : in out Tag_List_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if Name = "entity_type" then
         From.Entity_Type := Util.Beans.Objects.To_Unbounded_String (Value);
      elsif Name = "permission" then
         From.Permission := Util.Beans.Objects.To_Unbounded_String (Value);
      end if;
   end Set_Value;

   --  ------------------------------
   --  Set the entity type (database table) onto which the tags are associated.
   --  ------------------------------
   procedure Set_Entity_Type (Into  : in out Tag_List_Bean;
                              Table : in ADO.Schemas.Class_Mapping_Access) is
   begin
      Into.Entity_Type := Ada.Strings.Unbounded.To_Unbounded_String (Table.Table.all);
   end Set_Entity_Type;

   --  ------------------------------
   --  Set the permission to check before removing or adding a tag on the entity.
   --  ------------------------------
   procedure Set_Permission (Into       : in out Tag_List_Bean;
                             Permission : in String) is
   begin
      Into.Permission := Ada.Strings.Unbounded.To_Unbounded_String (Permission);
   end Set_Permission;

   --  ------------------------------
   --  Load the tags associated with the given database identifier.
   --  ------------------------------
   procedure Load_Tags (Into          : in out Tag_List_Bean;
                        Session       : in ADO.Sessions.Session;
                        For_Entity_Id : in ADO.Identifier) is
      use ADO.Sessions.Entities;

      Entity_Type : constant String := Ada.Strings.Unbounded.To_String (Into.Entity_Type);
      Kind        : constant ADO.Entity_Type := Find_Entity_Type (Session, Entity_Type);
      Query       : ADO.Queries.Context;
   begin
      Query.Set_Query (AWA.Tags.Models.Query_Tag_List);
      Query.Bind_Param ("entity_type", Kind);
      Query.Bind_Param ("entity_id", For_Entity_Id);
      declare
         Stmt : ADO.Statements.Query_Statement := Session.Create_Statement (Query);
      begin
         Stmt.Execute;

         while Stmt.Has_Elements loop
            Into.List.Append (Util.Beans.Objects.To_Object (Stmt.Get_String (0)));
            Stmt.Next;
         end loop;
      end;
   end Load_Tags;

   --  ------------------------------
   --  Set the list of tags to add.
   --  ------------------------------
   procedure Set_Added (Into  : in out Tag_List_Bean;
                        Tags  : in Util.Strings.Vectors.Vector) is
   begin
      Into.Added := Tags;
   end Set_Added;

   --  ------------------------------
   --  Set the list of tags to remove.
   --  ------------------------------
   procedure Set_Deleted (Into : in out Tag_List_Bean;
                          Tags : in Util.Strings.Vectors.Vector) is
   begin
      Into.Deleted := Tags;
   end Set_Deleted;

   --  ------------------------------
   --  Update the tags associated with the tag entity represented by <tt>For_Entity_Id</tt>.
   --  The list of tags defined by <tt>Set_Deleted</tt> are removed first and the list of
   --  tags defined by <tt>Set_Added</tt> are associated with the database entity.
   --  ------------------------------
   procedure Update_Tags (From          : in Tag_List_Bean;
                          For_Entity_Id : in ADO.Identifier) is
      use type AWA.Tags.Modules.Tag_Module_Access;

      Entity_Type : constant String := Ada.Strings.Unbounded.To_String (From.Entity_Type);
      Service     : AWA.Tags.Modules.Tag_Module_Access := From.Module;
   begin
      if Service = null then
         Service := AWA.Tags.Modules.Get_Tag_Module;
      end if;
      Service.Update_Tags (Id          => For_Entity_Id,
                           Entity_Type => Entity_Type,
                           Permission  => Ada.Strings.Unbounded.To_String (From.Permission),
                           Added       => From.Added,
                           Deleted     => From.Deleted);
   end Update_Tags;

   --  ------------------------------
   --  Create the tag list bean instance.
   --  ------------------------------
   function Create_Tag_List_Bean (Module : in AWA.Tags.Modules.Tag_Module_Access)
                                  return Util.Beans.Basic.Readonly_Bean_Access is
      Result : constant Tag_List_Bean_Access := new Tag_List_Bean;
   begin
      Result.Module := Module;
      return Result.all'Access;
   end Create_Tag_List_Bean;

   --  ------------------------------
   --  Search the tags that match the search string.
   --  ------------------------------
   procedure Search_Tags (Into    : in out Tag_Search_Bean;
                          Session : in ADO.Sessions.Session;
                          Search  : in String) is
      use ADO.Sessions.Entities;

      Entity_Type : constant String := Ada.Strings.Unbounded.To_String (Into.Entity_Type);
      Kind        : constant ADO.Entity_Type := Find_Entity_Type (Session, Entity_Type);
      Query       : ADO.Queries.Context;
   begin
      Query.Set_Query (AWA.Tags.Models.Query_Tag_Search);
      Query.Bind_Param ("entity_type", Kind);
      Query.Bind_Param ("search", Search & "%");
      declare
         Stmt : ADO.Statements.Query_Statement := Session.Create_Statement (Query);
      begin
         Stmt.Execute;

         while Stmt.Has_Elements loop
            Into.List.Append (Util.Beans.Objects.To_Object (Stmt.Get_String (0)));
            Stmt.Next;
         end loop;
      end;
   end Search_Tags;

   --  ------------------------------
   --  Set the value identified by the name.
   --  If the name cannot be found, the method should raise the No_Value
   --  exception.
   --  ------------------------------
   overriding
   procedure Set_Value (From  : in out Tag_Search_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if Name = "entity_type" then
         From.Entity_Type := Util.Beans.Objects.To_Unbounded_String (Value);

      elsif Name = "search" then
         declare
            Session : constant ADO.Sessions.Session := From.Module.Get_Session;
         begin
            From.Search_Tags (Session, Util.Beans.Objects.To_String (Value));
         end;
      end if;
   end Set_Value;

   --  ------------------------------
   --  Set the entity type (database table) onto which the tags are associated.
   --  ------------------------------
   procedure Set_Entity_Type (Into  : in out Tag_Search_Bean;
                              Table : in ADO.Schemas.Class_Mapping_Access) is
   begin
      Into.Entity_Type := Ada.Strings.Unbounded.To_Unbounded_String (Table.Table.all);
   end Set_Entity_Type;

   --  ------------------------------
   --  Create the tag search bean instance.
   --  ------------------------------
   function Create_Tag_Search_Bean (Module : in AWA.Tags.Modules.Tag_Module_Access)
                                  return Util.Beans.Basic.Readonly_Bean_Access is
      Result : constant Tag_Search_Bean_Access := new Tag_Search_Bean;
   begin
      Result.Module := Module;
      return Result.all'Access;
   end Create_Tag_Search_Bean;

   --  ------------------------------
   --  Set the value identified by the name.
   --  If the name cannot be found, the method should raise the No_Value
   --  exception.
   --  ------------------------------
   overriding
   procedure Set_Value (From  : in out Tag_Info_List_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if Name = "entity_type" then
         From.Entity_Type := Util.Beans.Objects.To_Unbounded_String (Value);
         declare
            Session : ADO.Sessions.Session := From.Module.Get_Session;
         begin
            From.Load_Tags (Session);
         end;
      end if;
   end Set_Value;

   --  ------------------------------
   --  Load the list of tags.
   --  ------------------------------
   procedure Load_Tags (Into    : in out Tag_Info_List_Bean;
                        Session : in out ADO.Sessions.Session) is
      use ADO.Sessions.Entities;

      Entity_Type : constant String := Ada.Strings.Unbounded.To_String (Into.Entity_Type);
      Kind        : constant ADO.Entity_Type := Find_Entity_Type (Session, Entity_Type);
      Query       : ADO.Queries.Context;
   begin
      Query.Set_Query (AWA.Tags.Models.Query_Tag_List_All);
      Query.Bind_Param ("entity_type", Kind);
      AWA.Tags.Models.List (Into, Session, Query);
   end Load_Tags;

   --  ------------------------------
   --  Create the tag info list bean instance.
   --  ------------------------------
   function Create_Tag_Info_List_Bean (Module : in AWA.Tags.Modules.Tag_Module_Access)
                                    return Util.Beans.Basic.Readonly_Bean_Access is
      Result : constant Tag_Info_List_Bean_Access := new Tag_Info_List_Bean;
   begin
      Result.Module := Module;
      return Result.all'Access;
   end Create_Tag_Info_List_Bean;

   --  ------------------------------
   --  Get the list of tags associated with the given entity.
   --  Returns null if the entity does not have any tag.
   --  ------------------------------
   function Get_Tags (From       : in Entity_Tag_Map;
                      For_Entity : in ADO.Identifier)
                      return Util.Beans.Lists.Strings.List_Bean_Access is
      Pos : constant Entity_Tag_Maps.Cursor := From.Tags.Find (For_Entity);
   begin
      if Entity_Tag_Maps.Has_Element (Pos) then
         return Entity_Tag_Maps.Element (Pos);
      else
         return null;
      end if;
   end Get_Tags;

   --  ------------------------------
   --  Get the list of tags associated with the given entity.
   --  Returns a null object if the entity does not have any tag.
   --  ------------------------------
   function Get_Tags (From       : in Entity_Tag_Map;
                      For_Entity : in ADO.Identifier)
                      return Util.Beans.Objects.Object is
      Pos : constant Entity_Tag_Maps.Cursor := From.Tags.Find (For_Entity);
   begin
      if Entity_Tag_Maps.Has_Element (Pos) then
         return Util.Beans.Objects.To_Object (Value   => Entity_Tag_Maps.Element (Pos).all'Access,
                                              Storage => Util.Beans.Objects.STATIC);
      else
         return Util.Beans.Objects.Null_Object;
      end if;
   end Get_Tags;

   --  ------------------------------
   --  Load the list of tags associated with a list of entities.
   --  ------------------------------
   procedure Load_Tags (Into        : in out Entity_Tag_Map;
                        Session     : in out ADO.Sessions.Session'Class;
                        Entity_Type : in String;
                        List        : in ADO.Utils.Identifier_Vector) is
      Query : ADO.Queries.Context;
      Kind  : ADO.Entity_Type;
   begin
      Into.Clear;
      if List.Is_Empty then
         return;
      end if;
      Kind := ADO.Sessions.Entities.Find_Entity_Type (Session, Entity_Type);
      Query.Set_Query (AWA.Tags.Models.Query_Tag_List_For_Entities);
      Query.Bind_Param ("entity_id_list", List);
      Query.Bind_Param ("entity_type", Kind);
      declare
         Stmt  : ADO.Statements.Query_Statement := Session.Create_Statement (Query);
         Id    : ADO.Identifier;
         List  : Util.Beans.Lists.Strings.List_Bean_Access;
         Pos   : Entity_Tag_Maps.Cursor;
      begin
         Stmt.Execute;
         while Stmt.Has_Elements loop
            Id := Stmt.Get_Identifier (0);
            Pos := Into.Tags.Find (Id);
            if not Entity_Tag_Maps.Has_Element (Pos) then
               List := new Util.Beans.Lists.Strings.List_Bean;
               Into.Tags.Insert (Id, List);
            else
               List := Entity_Tag_Maps.Element (Pos);
            end if;
            List.List.Append (Stmt.Get_String (1));
            Stmt.Next;
         end loop;
      end;
   end Load_Tags;

   --  ------------------------------
   --  Release the list of tags.
   --  ------------------------------
   procedure Clear (List : in out Entity_Tag_Map) is
      procedure Free is
        new Ada.Unchecked_Deallocation (Object => Util.Beans.Lists.Strings.List_Bean'Class,
                                        Name   => Util.Beans.Lists.Strings.List_Bean_Access);

      Pos  : Entity_Tag_Maps.Cursor;
      Tags : Util.Beans.Lists.Strings.List_Bean_Access;
   begin
      loop
         Pos := List.Tags.First;
         exit when not Entity_Tag_Maps.Has_Element (Pos);
         Tags := Entity_Tag_Maps.Element (Pos);
         List.Tags.Delete (Pos);
         Free (Tags);
      end loop;
   end Clear;

   --  ------------------------------
   --  Release the list of tags.
   --  ------------------------------
   overriding
   procedure Finalize (List : in out Entity_Tag_Map) is
   begin
      List.Clear;
   end Finalize;

end AWA.Tags.Beans;
