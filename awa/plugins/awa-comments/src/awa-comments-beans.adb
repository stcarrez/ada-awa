-----------------------------------------------------------------------
--  awa-comments-beans -- Beans for the comments module
--  Copyright (C) 2014 Stephane Carrez
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
with ADO.Sessions.Entities;
with ADO.Queries;
with ADO.Statements;
with ADO.Utils;
with AWA.Services.Contexts;
package body AWA.Comments.Beans is

   package ASC renames AWA.Services.Contexts;

   --  Get the value identified by the name.
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

   --  Set the value identified by the name.
   overriding
   procedure Set_Value (From  : in out Comment_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if Name = "comment" then
         From.Set_Message (Util.Beans.Objects.To_String (Value));

      elsif Name = "entity_type" then
         From.Entity_Type := Util.Beans.Objects.To_Unbounded_String (Value);

      elsif Name = "permission" then
         From.Permission := Util.Beans.Objects.To_Unbounded_String (Value);

      elsif Name = "status" then
         From.Set_Status (AWA.Comments.Models.Status_Type_Objects.To_Value (Value));

      elsif Name = "id" and not Util.Beans.Objects.Is_Empty (Value) then
         declare
            Ctx : constant ASC.Service_Context_Access := AWA.Services.Contexts.Current;
            DB  : constant ADO.Sessions.Session := AWA.Services.Contexts.Get_Session (Ctx);
            Id  : constant ADO.Identifier := ADO.Utils.To_Identifier (Value);
         begin
            From.Module.Load_Comment (From, Id);
         end;
      end if;
   end Set_Value;

   --  Create the comment.
   overriding
   procedure Create (Bean    : in out Comment_Bean;
                     Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
   begin
      Bean.Module.Create_Comment (Ada.Strings.Unbounded.To_String (Bean.Permission), Bean);
   end Create;

   --  Save the comment.
   overriding
   procedure Save (Bean    : in out Comment_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
   begin
      null;
   end Save;

   --  Delete the comment.
   overriding
   procedure Delete (Bean    : in out Comment_Bean;
                     Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
   begin
      null;
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
                            Session       : in ADO.Sessions.Session;
                            For_Entity_Id : in ADO.Identifier) is
      use ADO.Sessions.Entities;

      Entity_Type : constant String := Ada.Strings.Unbounded.To_String (Into.Entity_Type);
      Kind        : constant ADO.Entity_Type := Find_Entity_Type (Session, Entity_Type);
      Query       : ADO.Queries.Context;
   begin
      Query.Set_Query (AWA.Comments.Models.Query_Comment_List);
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
