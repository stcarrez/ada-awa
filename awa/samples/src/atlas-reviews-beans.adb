-----------------------------------------------------------------------
--  atlas-reviews-beans -- Beans for module reviews
--  Copyright (C) 2014 Stephane.Carrez
--  Written by Stephane.Carrez (Stephane.Carrez@gmail.com)
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
with ADO.Queries;
with ADO.Utils;
with ADO.Datasets;
with AWA.Services.Contexts;
package body Atlas.Reviews.Beans is

   package ASC renames AWA.Services.Contexts;

   --  ------------------------------
   --  Get the value identified by the name.
   --  ------------------------------
   overriding
   function Get_Value (From : in Review_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if From.Is_Null then
         return Util.Beans.Objects.Null_Object;
      else
         return Atlas.Reviews.Models.Review_Bean (From).Get_Value (Name);
      end if;
   end Get_Value;

   --  ------------------------------
   --  Set the value identified by the name.
   --  ------------------------------
   overriding
   procedure Set_Value (From  : in out Review_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if Name = "title" then
         From.Set_Title (Util.Beans.Objects.To_String (Value));
      elsif Name = "site" then
         From.Set_Site (Util.Beans.Objects.To_String (Value));
      elsif Name = "text" then
         From.Set_Text (Util.Beans.Objects.To_String (Value));
      elsif Name = "id" and not Util.Beans.Objects.Is_Empty (Value) then
         declare
            Ctx : constant ASC.Service_Context_Access := AWA.Services.Contexts.Current;
            DB  : ADO.Sessions.Session := AWA.Services.Contexts.Get_Session (Ctx);
            Id  : constant ADO.Identifier := ADO.Utils.To_Identifier (Value);
            Found : Boolean;
         begin
            From.Load (DB, Id, Found);
         end;
      end if;
   end Set_Value;

   overriding
   procedure Save (Bean : in out Review_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      pragma Unreferenced (Outcome);
   begin
      Bean.Module.Save (Bean);
   end Save;

   overriding
   procedure Delete (Bean : in out Review_Bean;
                     Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      pragma Unreferenced (Outcome);
   begin
      Bean.Module.Delete (Bean);
   end Delete;

   overriding
   procedure Load (Bean : in out Review_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      Session     : ADO.Sessions.Session := Bean.Module.Get_Session;
   begin
      null;
   end Load;

   --  ------------------------------
   --  Create the Review_Bean bean instance.
   --  ------------------------------
   function Create_Review_Bean (Module : in Atlas.Reviews.Modules.Review_Module_Access)
      return Util.Beans.Basic.Readonly_Bean_Access is
      Object : constant Review_Bean_Access := new Review_Bean;
   begin
      Object.Module := Module;
      return Object.all'Access;
   end Create_Review_Bean;

   --  ------------------------------
   --  Get the value identified by the name.
   --  ------------------------------
   overriding
   function Get_Value (From : in Review_List_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = "page" then
         return Util.Beans.Objects.To_Object (From.Page);
      elsif Name = "page_size" then
         return Util.Beans.Objects.To_Object (From.Page_Size);
      elsif Name = "count" then
         return Util.Beans.Objects.To_Object (From.Count);
      elsif Name = "reviews" then
         return Util.Beans.Objects.To_Object (Value   => From.Reviews_Bean,
                                              Storage => Util.Beans.Objects.STATIC);
      else
         return From.Reviews.Get_Value (Name);
      end if;
   end Get_Value;

   --  ------------------------------
   --  Set the value identified by the name.
   --  ------------------------------
   overriding
   procedure Set_Value (From  : in out Review_List_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if Name = "page" then
         From.Page := Util.Beans.Objects.To_Integer (Value);
      elsif Name = "page_size" then
         From.Page_Size := Util.Beans.Objects.To_Integer (Value);
      end if;
   end Set_Value;

   overriding
   procedure Load (Into    : in out Review_List_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      Session     : ADO.Sessions.Session := Into.Module.Get_Session;
      Query       : ADO.Queries.Context;
      Count_Query : ADO.Queries.Context;
      First       : constant Natural  := (Into.Page - 1) * Into.Page_Size;
      Last        : constant Positive := First + Into.Page_Size;
   begin
      Query.Set_Query (Atlas.Reviews.Models.Query_List);
      Count_Query.Set_Count_Query (Atlas.Reviews.Models.Query_List);
      Query.Bind_Param (Name => "first", Value => First);
      Query.Bind_Param (Name => "last", Value => Last);
      Atlas.Reviews.Models.List (Into.Reviews, Session, Query);
      Into.Count := ADO.Datasets.Get_Count (Session, Count_Query);
   end Load;

   --  ------------------------------
   --  Create the Review_List_Bean bean instance.
   --  ------------------------------
   function Create_Review_List_Bean (Module : in Atlas.Reviews.Modules.Review_Module_Access)
                                     return Util.Beans.Basic.Readonly_Bean_Access is
      Object  : constant Review_List_Bean_Access := new Review_List_Bean;
   begin
      Object.Module       := Module;
      Object.Reviews_Bean := Object.Reviews'Access;
      Object.Page_Size    := 20;
      Object.Page         := 1;
      Object.Count        := 0;
      return Object.all'Access;
   end Create_Review_List_Bean;

end Atlas.Reviews.Beans;
