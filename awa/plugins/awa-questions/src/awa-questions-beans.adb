-----------------------------------------------------------------------
--  awa-questions-beans -- Beans for module questions
--  Copyright (C) 2012, 2013 Stephane Carrez
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
with ADO.Queries;
with ADO.Sessions;

with AWA.Services.Contexts;
with AWA.Questions.Services;
package body AWA.Questions.Beans is

   --  ------------------------------
   --  Get the value identified by the name.
   --  ------------------------------
   overriding
   function Get_Value (From : in Question_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = "count" then
         return Util.Beans.Objects.To_Object (From.Count);
      else
         return Util.Beans.Objects.Null_Object;
      end if;
   end Get_Value;

   --  ------------------------------
   --  Set the value identified by the name.
   --  ------------------------------
   overriding
   procedure Set_Value (From  : in out Question_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if Name = "count" then
         From.Count := Util.Beans.Objects.To_Integer (Value);
      end if;
   end Set_Value;

   procedure Save (Bean : in out Question_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      Manager : constant Services.Question_Service_Access := Bean.Module.Get_Question_Manager;
   begin
      Manager.Save_Question (Bean);
   end Save;

   procedure Delete (Bean : in out Question_Bean;
                     Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
   begin
      null;
   end Delete;

   --  ------------------------------
   --  Create the Question_Bean bean instance.
   --  ------------------------------
   function Create_Question_Bean (Module : in AWA.Questions.Modules.Question_Module_Access)
      return Util.Beans.Basic.Readonly_Bean_Access is
      Object : constant Question_Bean_Access := new Question_Bean;
   begin
      Object.Module := Module;
      return Object.all'Access;
   end Create_Question_Bean;

   --  ------------------------------
   --  Create the Question_Info_List_Bean bean instance.
   --  ------------------------------
   function Create_Question_List_Bean (Module : in AWA.Questions.Modules.Question_Module_Access)
                                       return Util.Beans.Basic.Readonly_Bean_Access is
      use AWA.Questions.Models;
      use AWA.Services;

      Ctx     : constant Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      Object  : constant Question_Info_List_Bean_Access := new Question_Info_List_Bean;
      Session : ADO.Sessions.Session := Module.Get_Session;
      Query   : ADO.Queries.Context;
   begin
      Query.Set_Query (AWA.Questions.Models.Query_Question_List);
      AWA.Questions.Models.List (Object.all, Session, Query);
      return Object.all'Access;
   end Create_Question_List_Bean;

end AWA.Questions.Beans;
