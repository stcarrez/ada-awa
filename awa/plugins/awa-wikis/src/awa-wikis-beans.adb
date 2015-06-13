-----------------------------------------------------------------------
--  awa-wikis-beans -- Beans for module wikis
--  Copyright (C) 2015 Stephane Carrez
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
with ADO.Utils;
with ADO.Queries;
with ADO.Sessions;
with ADO.Sessions.Entities;

with AWA.Services;
with AWA.Services.Contexts;

package body AWA.Wikis.Beans is

   --  ------------------------------
   --  Get the value identified by the name.
   --  ------------------------------
   overriding
   function Get_Value (From : in Wiki_Space_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if From.Is_Null then
         return Util.Beans.Objects.Null_Object;
      else
         return AWA.Wikis.Models.Wiki_Space_Bean (From).Get_Value (Name);
      end if;
   end Get_Value;

   --  ------------------------------
   --  Set the value identified by the name.
   --  ------------------------------
   overriding
   procedure Set_Value (From  : in out Wiki_Space_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if Name = "name" then
         From.Set_Name (Util.Beans.Objects.To_String (Value));
      end if;
   end Set_Value;

   --  ------------------------------
   --  Create or save the wiki space.
   --  ------------------------------
   overriding
   procedure Save (Bean    : in out Wiki_Space_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      pragma Unreferenced (Outcome);
   begin
      Bean.Service.Save_Wiki_Space (Bean);
   end Save;

   --  Delete the wiki space.
   procedure Delete (Bean    : in out Wiki_Space_Bean;
                     Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
   begin
      null;
   end Delete;

   --  ------------------------------
   --  Create the Wiki_Space_Bean bean instance.
   --  ------------------------------
   function Create_Wiki_Space_Bean (Module : in AWA.Wikis.Modules.Wiki_Module_Access)
                                    return Util.Beans.Basic.Readonly_Bean_Access is
      Object : constant Wiki_Space_Bean_Access := new Wiki_Space_Bean;
   begin
      Object.Service   := Module;
      return Object.all'Access;
   end Create_Wiki_Space_Bean;

   --  ------------------------------
   --  Get the value identified by the name.
   --  ------------------------------
   overriding
   function Get_Value (From : in Wiki_Page_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if From.Is_Null then
         return Util.Beans.Objects.Null_Object;
      else
         return AWA.Wikis.Models.Wiki_Page_Bean (From).Get_Value (Name);
      end if;
   end Get_Value;

   --  ------------------------------
   --  Set the value identified by the name.
   --  ------------------------------
   overriding
   procedure Set_Value (From  : in out Wiki_Page_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if Name = "name" then
         From.Set_Name (Util.Beans.Objects.To_String (Value));
      elsif Name = "title" then
         From.Set_Title (Util.Beans.Objects.To_String (Value));
      end if;
   end Set_Value;

   --  ------------------------------
   --  Create or save the wiki page.
   --  ------------------------------
   overriding
   procedure Save (Bean    : in out Wiki_Page_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
   begin
      if Bean.Is_Inserted then
         Bean.Service.Save (Bean);
      else
         Bean.Service.Create_Wiki_Page (Bean.Wiki_Space, Bean);
      end if;
   end Save;

   --  Delete the wiki page.
   overriding
   procedure Delete (Bean    : in out Wiki_Page_Bean;
                     Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
   begin
      null;
   end Delete;

   --  ------------------------------
   --  Create the Wiki_Page_Bean bean instance.
   --  ------------------------------
   function Create_Wiki_Page_Bean (Module : in AWA.Wikis.Modules.Wiki_Module_Access)
                                   return Util.Beans.Basic.Readonly_Bean_Access is
      Object : constant Wiki_Page_Bean_Access := new Wiki_Page_Bean;
   begin
      Object.Service   := Module;
      return Object.all'Access;
   end Create_Wiki_Page_Bean;

   --  ------------------------------
   --  Load the list of wikis.
   --  ------------------------------
   procedure Load_Wikis (List : in Wiki_Admin_Bean) is
      use AWA.Wikis.Models;
      use AWA.Services;

      Ctx     : constant Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      User    : constant ADO.Identifier := Ctx.Get_User_Identifier;
      Session : ADO.Sessions.Session := List.Module.Get_Session;
      Query   : ADO.Queries.Context;
   begin
      Query.Set_Query (AWA.Wikis.Models.Query_Wiki_List);
      Query.Bind_Param ("user_id", User);
      ADO.Sessions.Entities.Bind_Param (Params  => Query,
                                        Name    => "table",
                                        Table   => AWA.Wikis.Models.WIKI_SPACE_TABLE,
                                        Session => Session);
      AWA.Wikis.Models.List (List.Wiki_List_Bean.all, Session, Query);
      List.Flags (INIT_WIKI_LIST) := True;
   end Load_Wikis;

   --  ------------------------------
   --  Get the wiki space identifier.
   --  ------------------------------
   function Get_Wiki_Id (List : in Wiki_Admin_Bean) return ADO.Identifier is
      use type ADO.Identifier;
   begin
      if List.Wiki_Id = ADO.NO_IDENTIFIER then
         if not List.Flags (INIT_WIKI_LIST) then
            Load_Wikis (List);
         end if;
         if not List.Wiki_List.List.Is_Empty then
            return List.Wiki_List.List.Element (0).Id;
         end if;
      end if;
      return List.Wiki_Id;
   end Get_Wiki_Id;

   overriding
   function Get_Value (List : in Wiki_Admin_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = "wikis" then
         if not List.Init_Flags (INIT_WIKI_LIST) then
            Load_Wikis (List);
         end if;
         return Util.Beans.Objects.To_Object (Value   => List.Wiki_List_Bean,
                                              Storage => Util.Beans.Objects.STATIC);

      elsif Name = "id" then
         declare
            use type ADO.Identifier;

            Id : constant ADO.Identifier := List.Get_Wiki_Id;
         begin
            if Id = ADO.NO_IDENTIFIER then
               return Util.Beans.Objects.Null_Object;
            else
               return Util.Beans.Objects.To_Object (Long_Long_Integer (Id));
            end if;
         end;
      else
         return Util.Beans.Objects.Null_Object;
      end if;
   end Get_Value;

   --  ------------------------------
   --  Set the value identified by the name.
   --  ------------------------------
   overriding
   procedure Set_Value (From  : in out Wiki_Admin_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if Name = "id" and not Util.Beans.Objects.Is_Empty (Value) then
         From.Wiki_Id := ADO.Utils.To_Identifier (Value);
      end if;
   end Set_Value;

   --  ------------------------------
   --  Create the Wiki_Admin_Bean bean instance.
   --  ------------------------------
   function Create_Wiki_Admin_Bean (Module : in AWA.Wikis.Modules.Wiki_Module_Access)
                                    return Util.Beans.Basic.Readonly_Bean_Access is
      Object  : constant Wiki_Admin_Bean_Access := new Wiki_Admin_Bean;
   begin
      Object.Module            := Module;
      Object.Flags             := Object.Init_Flags'Access;
      Object.Wiki_List_Bean    := Object.Wiki_List'Access;
      return Object.all'Access;
   end Create_Wiki_Admin_Bean;

end AWA.Wikis.Beans;
