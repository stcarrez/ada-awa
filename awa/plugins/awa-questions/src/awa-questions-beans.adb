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
with ADO.Sessions.Entities;

with AWA.Services.Contexts;
package body AWA.Questions.Beans is

   --  ------------------------------
   --  Get the value identified by the name.
   --  ------------------------------
   overriding
   function Get_Value (From : in Question_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if From.Is_Null then
         return Util.Beans.Objects.Null_Object;
      else
         return AWA.Questions.Models.Question_Bean (From).Get_Value (Name);
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
      if Name = "title" then
         From.Set_Title (Util.Beans.Objects.To_String (Value));

      elsif Name = "description" then
         From.Set_Description (Util.Beans.Objects.To_String (Value));

      elsif Name = "id" and not Util.Beans.Objects.Is_Empty (Value) then
            From.Service.Load_Question (From,
                                        ADO.Identifier (Util.Beans.Objects.To_Integer (Value)));
      end if;
   end Set_Value;

   --  ------------------------------
   --  Create or save the question.
   --  ------------------------------
   procedure Save (Bean    : in out Question_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      pragma Unreferenced (Outcome);
   begin
      Bean.Service.Save_Question (Bean);
   end Save;

   --  ------------------------------
   --  Delete the question.
   --  ------------------------------
   procedure Delete (Bean    : in out Question_Bean;
                     Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      pragma Unreferenced (Outcome);
   begin
      Bean.Service.Delete_Question (Bean);
   end Delete;

   --  ------------------------------
   --  Create the Question_Bean bean instance.
   --  ------------------------------
   function Create_Question_Bean (Module : in AWA.Questions.Modules.Question_Module_Access)
      return Util.Beans.Basic.Readonly_Bean_Access is
      Object : constant Question_Bean_Access := new Question_Bean;
   begin
      Object.Service := Module.Get_Question_Manager;
      return Object.all'Access;
   end Create_Question_Bean;


   --  Get the value identified by the name.
   overriding
   function Get_Value (From : in Answer_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = "question_id" then
         return Util.Beans.Objects.To_Object (Long_Long_Integer (From.Question.Get_Id));
      elsif From.Is_Null then
         return Util.Beans.Objects.Null_Object;
      else
         return AWA.Questions.Models.Answer_Bean (From).Get_Value (Name);
      end if;
   end Get_Value;

   --  ------------------------------
   --  Set the value identified by the name.
   --  ------------------------------
   overriding
   procedure Set_Value (From  : in out Answer_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if Name = "id" and not Util.Beans.Objects.Is_Null (Value) then
         From.Service.Load_Answer (From, From.Question,
                                   ADO.Identifier (Util.Beans.Objects.To_Integer (Value)));

      elsif Name = "answer" then
         From.Set_Answer (Util.Beans.Objects.To_String (Value));

      elsif Name = "question_id" and not Util.Beans.Objects.Is_Null (Value) then
         From.Service.Load_Question (From.Question,
                                     ADO.Identifier (Util.Beans.Objects.To_Integer (Value)));
      end if;
   end Set_Value;

   --  ------------------------------
   --  Create or save the answer.
   --  ------------------------------
   procedure Save (Bean    : in out Answer_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      pragma Unreferenced (Outcome);
   begin
      Bean.Service.Save_Answer (Question => Bean.Question,
                                Answer   => Bean);
   end Save;

   --  Delete the question.
   procedure Delete (Bean    : in out Answer_Bean;
                     Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
   begin
      null;
   end Delete;

   --  ------------------------------
   --  Create the answer bean instance.
   --  ------------------------------
   function Create_Answer_Bean (Module : in AWA.Questions.Modules.Question_Module_Access)
                                return Util.Beans.Basic.Readonly_Bean_Access is
      Object : constant Answer_Bean_Access := new Answer_Bean;
   begin
      Object.Service := Module.Get_Question_Manager;
      return Object.all'Access;
   end Create_Answer_Bean;

   --  ------------------------------
   --  Create the Question_Info_List_Bean bean instance.
   --  ------------------------------
   function Create_Question_List_Bean (Module : in AWA.Questions.Modules.Question_Module_Access)
                                       return Util.Beans.Basic.Readonly_Bean_Access is
      use AWA.Questions.Models;
      use AWA.Services;

      Object  : constant Question_Info_List_Bean_Access := new Question_Info_List_Bean;
      Session : ADO.Sessions.Session := Module.Get_Session;
      Query   : ADO.Queries.Context;
   begin
      Query.Set_Query (AWA.Questions.Models.Query_Question_List);
      ADO.Sessions.Entities.Bind_Param (Params  => Query,
                                        Name    => "entity_type",
                                        Table   => AWA.Questions.Models.QUESTION_TABLE,
                                        Session => Session);
      AWA.Questions.Models.List (Object.all, Session, Query);
      return Object.all'Access;
   end Create_Question_List_Bean;

   --  ------------------------------
   --  Get the value identified by the name.
   --  ------------------------------
   overriding
   function Get_Value (From : in Question_Display_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = "answers" then
         return Util.Beans.Objects.To_Object (Value   => From.Answer_List_Bean,
                                              Storage => Util.Beans.Objects.STATIC);

      elsif Name = "question" then
         return Util.Beans.Objects.To_Object (Value   => From.Question_Bean,
                                              Storage => Util.Beans.Objects.STATIC);

      else
         return Util.Beans.Objects.Null_Object;
      end if;
   end Get_Value;

   --  ------------------------------
   --  Set the value identified by the name.
   --  ------------------------------
   overriding
   procedure Set_Value (From  : in out Question_Display_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if Name = "id" and not Util.Beans.Objects.Is_Empty (Value) then
         declare
            package ASC renames AWA.Services.Contexts;
            use AWA.Questions.Models;
            use AWA.Services;

            Session : ADO.Sessions.Session := From.Service.Get_Session;
            Query   : ADO.Queries.Context;
            List    : AWA.Questions.Models.Question_Display_Info_List_Bean;
            Ctx     : constant ASC.Service_Context_Access := ASC.Current;
         begin
            Query.Set_Query (AWA.Questions.Models.Query_Question_Info);
            Query.Bind_Param ("question_id", Util.Beans.Objects.To_Integer (Value));
            Query.Bind_Param ("user_id", Ctx.Get_User_Identifier);
            ADO.Sessions.Entities.Bind_Param (Params  => Query,
                                              Name    => "entity_type",
                                              Table   => AWA.Questions.Models.QUESTION_TABLE,
                                              Session => Session);
            AWA.Questions.Models.List (List, Session, Query);
            if not List.List.Is_Empty then
               From.Question := List.List.Element (0);
            end if;
            Query.Clear;
            Query.Bind_Param ("question_id", Util.Beans.Objects.To_Integer (Value));
            Query.Bind_Param ("user_id", Ctx.Get_User_Identifier);
            ADO.Sessions.Entities.Bind_Param (Params  => Query,
                                              Name    => "entity_type",
                                              Table   => AWA.Questions.Models.ANSWER_TABLE,
                                              Session => Session);

            Query.Set_Query (AWA.Questions.Models.Query_Answer_List);
            AWA.Questions.Models.List (From.Answer_List, Session, Query);
         end;
      end if;
   end Set_Value;

   --  ------------------------------
   --  Create the Question_Display_Bean bean instance.
   --  ------------------------------
   function Create_Question_Display_Bean (Module : in AWA.Questions.Modules.Question_Module_Access)
                                       return Util.Beans.Basic.Readonly_Bean_Access is
      Object  : constant Question_Display_Bean_Access := new Question_Display_Bean;
   begin
      Object.Service          := Module.Get_Question_Manager;
      Object.Question_Bean    := Object.Question'Access;
      Object.Answer_List_Bean := Object.Answer_List'Access;
      return Object.all'Access;
   end Create_Question_Display_Bean;

end AWA.Questions.Beans;
