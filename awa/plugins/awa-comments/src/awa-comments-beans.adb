-----------------------------------------------------------------------
--  awa-comments-beans -- Beans for the comments module
--  Copyright (C) 2014, 2015, 2016, 2017, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with ADO.Sessions.Entities;
with ADO.Queries;
with ADO.Utils;
with ADO.Parameters;
with AWA.Helpers.Requests;
package body AWA.Comments.Beans is

   use Ada.Strings.Unbounded;

   --  ------------------------------
   --  Get the value identified by the name.
   --  ------------------------------
   overriding
   function Get_Value (From : in Comment_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if From.Is_Null then
         return Util.Beans.Objects.Null_Object;
      else
         return AWA.Comments.Models.Comment_Bean (From).Get_Value (Name);
      end if;
   end Get_Value;

   --  ------------------------------
   --  Set the value identified by the name.
   --  ------------------------------
   overriding
   procedure Set_Value (From  : in out Comment_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if Name = "entity_type" then
         From.Entity_Type := Util.Beans.Objects.To_Unbounded_String (Value);

      elsif Name = "entity_id" then
         declare
            use type ADO.Identifier;

            Id : constant ADO.Identifier := ADO.Utils.To_Identifier (Value);
         begin
            if Id /= ADO.NO_IDENTIFIER then
               From.Set_Entity_Id (ADO.Utils.To_Identifier (Value));
            end if;
         end;

      elsif Name = "permission" then
         From.Permission := Util.Beans.Objects.To_Unbounded_String (Value);

      elsif Name = "id" and then not Util.Beans.Objects.Is_Empty (Value) then
         declare
            Id  : constant ADO.Identifier := ADO.Utils.To_Identifier (Value);
         begin
            From.Module.Load_Comment (From, Id);
         end;
      else
         AWA.Comments.Models.Comment_Bean (From).Set_Value (Name, Value);
      end if;
   end Set_Value;

   --  ------------------------------
   --  Create the comment.
   --  ------------------------------
   overriding
   procedure Create (Bean    : in out Comment_Bean;
                     Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      pragma Unreferenced (Outcome);
   begin
      Bean.Module.Create_Comment (Permission  => To_String (Bean.Permission),
                                  Entity_Type => To_String (Bean.Entity_Type),
                                  Comment     => Bean);
   end Create;

   --  ------------------------------
   --  Save the comment.
   --  ------------------------------
   overriding
   procedure Save (Bean    : in out Comment_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      pragma Unreferenced (Outcome);
   begin
      Bean.Module.Update_Comment (Permission  => To_String (Bean.Permission),
                                  Comment     => Bean);
   end Save;

   --  ------------------------------
   --  Publish or not the comment.
   --  ------------------------------
   overriding
   procedure Publish (Bean    : in out Comment_Bean;
                      Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      pragma Unreferenced (Outcome);

      Id     : constant ADO.Identifier := Helpers.Requests.Get_Parameter ("id");
      Value  : constant Util.Beans.Objects.Object := Helpers.Requests.Get_Parameter ("status");
   begin
      Bean.Module.Publish_Comment (Permission  => To_String (Bean.Permission),
                                   Id          => Id,
                                   Status      => Models.Status_Type_Objects.To_Value (Value),
                                   Comment     => Bean);
   end Publish;

   --  ------------------------------
   --  Delete the comment.
   --  ------------------------------
   overriding
   procedure Delete (Bean    : in out Comment_Bean;
                     Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      pragma Unreferenced (Outcome);
   begin
      Bean.Module.Delete_Comment (Ada.Strings.Unbounded.To_String (Bean.Permission), Bean);
   end Delete;

   --  ------------------------------
   --  Create a new comment bean instance.
   --  ------------------------------
   function Create_Comment_Bean (Module : in AWA.Comments.Modules.Comment_Module_Access)
                                 return Util.Beans.Basic.Readonly_Bean_Access is
      Result : constant Comment_Bean_Access := new Comment_Bean;
   begin
      Result.Module := Module;
      return Result.all'Access;
   end Create_Comment_Bean;

   --  ------------------------------
   --  Set the value identified by the name.
   --  If the name cannot be found, the method should raise the No_Value
   --  exception.
   --  ------------------------------
   overriding
   procedure Set_Value (From  : in out Comment_List_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if Name = "entity_type" then
         From.Entity_Type := Util.Beans.Objects.To_Unbounded_String (Value);
      elsif Name = "permission" then
         From.Permission := Util.Beans.Objects.To_Unbounded_String (Value);
      elsif Name = "entity_id" then
         From.Load_Comments (ADO.Utils.To_Identifier (Value));
      elsif Name = "sort" then
         if Util.Beans.Objects.To_String (Value) = "oldest" then
            From.Oldest_First := True;
         else
            From.Oldest_First := False;
         end if;
      elsif Name = "status" then
         if Util.Beans.Objects.To_String (Value) = "published" then
            From.Publish_Only := True;
         else
            From.Publish_Only := False;
         end if;
      end if;
   end Set_Value;

   --  ------------------------------
   --  Set the entity type (database table) with which the comments are associated.
   --  ------------------------------
   procedure Set_Entity_Type (Into  : in out Comment_List_Bean;
                              Table : in ADO.Schemas.Class_Mapping_Access) is
   begin
      Into.Entity_Type := Ada.Strings.Unbounded.To_Unbounded_String (Table.Table.all);
   end Set_Entity_Type;

   --  ------------------------------
   --  Set the permission to check before removing or adding a comment on the entity.
   --  ------------------------------
   procedure Set_Permission (Into       : in out Comment_List_Bean;
                             Permission : in String) is
   begin
      Into.Permission := Ada.Strings.Unbounded.To_Unbounded_String (Permission);
   end Set_Permission;

   --  ------------------------------
   --  Load the comments associated with the given database identifier.
   --  ------------------------------
   procedure Load_Comments (Into          : in out Comment_List_Bean;
                            For_Entity_Id : in ADO.Identifier) is
      Session : ADO.Sessions.Session := Into.Module.Get_Session;
   begin
      Into.Load_Comments (Session, For_Entity_Id);
   end Load_Comments;

   --  ------------------------------
   --  Load the comments associated with the given database identifier.
   --  ------------------------------
   procedure Load_Comments (Into          : in out Comment_List_Bean;
                            Session       : in out ADO.Sessions.Session;
                            For_Entity_Id : in ADO.Identifier) is
      use ADO.Sessions.Entities;

      Entity_Type : constant String := Ada.Strings.Unbounded.To_String (Into.Entity_Type);
      Kind        : constant ADO.Entity_Type := Find_Entity_Type (Session, Entity_Type);
      Query       : ADO.Queries.Context;
   begin
      Into.Entity_Id := For_Entity_Id;
      if Into.Publish_Only then
         Query.Set_Query (AWA.Comments.Models.Query_Comment_List);
         Query.Bind_Param ("status", Integer (Models.Status_Type'Pos (Models.COMMENT_PUBLISHED)));
      else
         Query.Set_Query (AWA.Comments.Models.Query_All_Comment_List);
      end if;
      Query.Bind_Param ("entity_type", Kind);
      Query.Bind_Param ("entity_id", For_Entity_Id);
      if Into.Oldest_First then
         Query.Bind_Param ("sort", ADO.Parameters.Token '("ASC"));
      else
         Query.Bind_Param ("sort", ADO.Parameters.Token '("DESC"));
      end if;
      AWA.Comments.Models.List (Into, Session, Query);
   end Load_Comments;

   --  ------------------------------
   --  Create the comment list bean instance.
   --  ------------------------------
   function Create_Comment_List_Bean (Module : in AWA.Comments.Modules.Comment_Module_Access)
                                      return Util.Beans.Basic.Readonly_Bean_Access is
      Result : constant Comment_List_Bean_Access := new Comment_List_Bean;
   begin
      Result.Module := Module;
      return Result.all'Access;
   end Create_Comment_List_Bean;

end AWA.Comments.Beans;
