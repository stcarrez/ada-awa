-----------------------------------------------------------------------
--  awa-questions-beans -- Beans for module questions
--  Copyright (C) 2012, 2013, 2015, 2017, 2018, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with ADO.Queries;
with ADO.Utils;
with ADO.Sessions;

with AWA.Tags.Modules;
with AWA.Services.Contexts;
package body AWA.Questions.Beans is

   use type ADO.Identifier;

   package ASC renames AWA.Services.Contexts;

   --  ------------------------------
   --  Get the value identified by the name.
   --  ------------------------------
   overriding
   function Get_Value (From : in Question_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = "tags" then
         return Util.Beans.Objects.To_Object (From.Tags_Bean, Util.Beans.Objects.STATIC);
      elsif From.Is_Null then
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

      elsif Name = "id" and then not Util.Beans.Objects.Is_Empty (Value) then
         From.Set_Id (ADO.Utils.To_Identifier (Value));
         From.Service.Load_Question (From, From.Get_Id, From.Found);
      end if;

   exception
      when Constraint_Error =>
         From.Set_Id (ADO.NO_IDENTIFIER);

   end Set_Value;

   --  ------------------------------
   --  Load question.
   --  ------------------------------
   overriding
   procedure Load (Bean    : in out Question_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      Ctx   : constant ASC.Service_Context_Access := ASC.Current;
      DB    : constant ADO.Sessions.Session := ASC.Get_Session (Ctx);
   begin
      Bean.Service.Load_Question (Bean, Bean.Get_Id, Bean.Found);
      if not Bean.Found then
         Outcome := Ada.Strings.Unbounded.To_Unbounded_String ("not-found");
      else
         Outcome := Ada.Strings.Unbounded.To_Unbounded_String ("loaded");
         Bean.Tags.Load_Tags (DB, Bean.Get_Id);
      end if;
   end Load;

   --  ------------------------------
   --  Create or save the question.
   --  ------------------------------
   overriding
   procedure Save (Bean    : in out Question_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      pragma Unreferenced (Outcome);
   begin
      Bean.Service.Save_Question (Bean);
      Bean.Tags.Update_Tags (Bean.Get_Id);
   end Save;

   --  ------------------------------
   --  Delete the question.
   --  ------------------------------
   overriding
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
      Object.Service   := Module;
      Object.Tags_Bean := Object.Tags'Access;
      Object.Tags.Set_Entity_Type (AWA.Questions.Models.QUESTION_TABLE);
      Object.Tags.Set_Permission ("question-edit");
      return Object.all'Access;
   end Create_Question_Bean;

   --  ------------------------------
   --  Get the value identified by the name.
   --  ------------------------------
   overriding
   function Get_Value (From : in Answer_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = "question_id" then
         return Util.Beans.Objects.To_Object (Long_Long_Integer (From.Question.Get_Id));
      elsif From.Is_Null then
         return Util.Beans.Objects.Null_Object;
      elsif Name = "title" or else Name = "description" then
         return From.Question.Get_Value (Name);
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
      if Name = "id" and then not Util.Beans.Objects.Is_Empty (Value) then
         From.Set_Id (ADO.Utils.To_Identifier (Value));
         From.Service.Load_Answer (From, From.Question, From.Get_Id, From.Found);

      elsif Name = "answer" then
         From.Set_Answer (Util.Beans.Objects.To_String (Value));

      elsif Name = "question_id" and then not Util.Beans.Objects.Is_Null (Value) then
         From.Question.Set_Id (ADO.Utils.To_Identifier (Value));
         From.Service.Load_Question (From.Question, From.Question_Id, From.Found);

      end if;

   exception
      when Constraint_Error =>
         null;
   end Set_Value;

   --  ------------------------------
   --  Load the answer.
   --  ------------------------------
   overriding
   procedure Load (Bean    : in out Answer_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      Found : Boolean;
   begin
      if not Bean.is_Null and then Bean.Get_Id /= ADO.NO_IDENTIFIER then
         Bean.Service.Load_Answer (Bean, Bean.Question, Bean.Get_Id, Found);
      elsif not Bean.Question.Is_Null and then Bean.Question.Get_Id /= ADO.NO_IDENTIFIER then
         Bean.Service.Load_Question (Bean.Question, Bean.Question_Id, Found);
      else
         Found := False;
      end if;
      if not Found then
         Outcome := Ada.Strings.Unbounded.To_Unbounded_String ("not-found");
      else
         Outcome := Ada.Strings.Unbounded.To_Unbounded_String ("loaded");
      end if;
   end Load;

   --  ------------------------------
   --  Create or save the answer.
   --  ------------------------------
   overriding
   procedure Save (Bean    : in out Answer_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      pragma Unreferenced (Outcome);
   begin
      Bean.Service.Save_Answer (Question => Bean.Question,
                                Answer   => Bean);
   end Save;

   --  ------------------------------
   --  Delete the question.
   --  ------------------------------
   overriding
   procedure Delete (Bean    : in out Answer_Bean;
                     Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      pragma Unreferenced (Outcome);
   begin
      Bean.Service.Delete_Answer (Answer => Bean);
   end Delete;

   --  ------------------------------
   --  Create the answer bean instance.
   --  ------------------------------
   function Create_Answer_Bean (Module : in AWA.Questions.Modules.Question_Module_Access)
                                return Util.Beans.Basic.Readonly_Bean_Access is
      Object : constant Answer_Bean_Access := new Answer_Bean;
   begin
      Object.Service := Module;
      return Object.all'Access;
   end Create_Answer_Bean;

   --  ------------------------------
   --  Get the value identified by the name.
   --  ------------------------------
   overriding
   function Get_Value (From : in Question_List_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
      Pos : Natural;
   begin
      if Name = "tags" then
         Pos := From.Questions.Get_Row_Index;
         if Pos = 0 then
            return Util.Beans.Objects.Null_Object;
         end if;
         declare
            Item : constant Models.Question_Info := From.Questions.List.Element (Pos);
         begin
            return From.Tags.Get_Tags (Item.Id);
         end;
      elsif Name = "questions" then
         return Util.Beans.Objects.To_Object (Value   => From.Questions_Bean,
                                              Storage => Util.Beans.Objects.STATIC);
      elsif Name = "tag" then
         return Util.Beans.Objects.To_Object (From.Tag);
      else
         return From.Questions.Get_Value (Name);
      end if;
   end Get_Value;

   --  ------------------------------
   --  Set the value identified by the name.
   --  ------------------------------
   overriding
   procedure Set_Value (From  : in out Question_List_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if Name = "tag" then
         From.Tag := Util.Beans.Objects.To_Unbounded_String (Value);
      end if;
   end Set_Value;

   --  ------------------------------
   --  Load the list of question.  If a tag was set, filter the list of questions with the tag.
   --  ------------------------------
   procedure Load_List (Into : in out Question_List_Bean) is
      use AWA.Questions.Models;

      Session : ADO.Sessions.Session := Into.Service.Get_Session;
      Query   : ADO.Queries.Context;
      Tag_Id  : ADO.Identifier;
   begin
      AWA.Tags.Modules.Find_Tag_Id (Session, Ada.Strings.Unbounded.To_String (Into.Tag), Tag_Id);
      if Tag_Id /= ADO.NO_IDENTIFIER then
         Query.Set_Query (AWA.Questions.Models.Query_Question_Tag_List);
         Query.Bind_Param (Name => "tag", Value => Tag_Id);
      else
         Query.Set_Query (AWA.Questions.Models.Query_Question_List);
      end if;
      AWA.Questions.Models.List (Into.Questions, Session, Query);
      declare
         List : ADO.Utils.Identifier_Vector;
         Iter : Question_Info_Vectors.Cursor := Into.Questions.List.First;
      begin
         while Question_Info_Vectors.Has_Element (Iter) loop
            List.Append (Question_Info_Vectors.Element (Iter).Id);
            Question_Info_Vectors.Next (Iter);
         end loop;
         Into.Tags.Load_Tags (Session, AWA.Questions.Models.QUESTION_TABLE.Table.all,
                              List);
      end;
   end Load_List;

   --  ------------------------------
   --  Load the list of questions.
   --  ------------------------------
   overriding
   procedure Load (Bean    : in out Question_List_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      pragma Unreferenced (Outcome);
   begin
      Bean.Load_List;
   end Load;

   --  ------------------------------
   --  Create the Question_Info_List_Bean bean instance.
   --  ------------------------------
   function Create_Question_List_Bean (Module : in AWA.Questions.Modules.Question_Module_Access)
                                       return Util.Beans.Basic.Readonly_Bean_Access is
      Object  : constant Question_List_Bean_Access := new Question_List_Bean;
   begin
      Object.Service := Module;
      Object.Questions_Bean := Object.Questions'Unchecked_Access;
      Object.Load_List;
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

      elsif Name = "tags" then
         return Util.Beans.Objects.To_Object (Value   => From.Tags_Bean,
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
      if Name = "id" and then not Util.Beans.Objects.Is_Empty (Value) then
         From.Id := ADO.Utils.To_Identifier (Value);
         From.Answer := Get_Answer_Bean ("answer");
         From.Answer.Set_Value ("question_id", Value);
      end if;

   exception
      when Constraint_Error =>
         null;
   end Set_Value;

   --  ------------------------------
   --  Load the question and its answers.
   --  ------------------------------
   overriding
   procedure Load (Bean    : in out Question_Display_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      package ASC renames AWA.Services.Contexts;
      use AWA.Questions.Models;

      Session : ADO.Sessions.Session := Bean.Service.Get_Session;
      Query   : ADO.Queries.Context;
      List    : AWA.Questions.Models.Question_Display_Info_List_Bean;
      Ctx     : constant ASC.Service_Context_Access := ASC.Current;
   begin
      Query.Set_Query (AWA.Questions.Models.Query_Question_Info);
      Query.Bind_Param ("question_id", Bean.Id);
      Query.Bind_Param ("user_id", Ctx.Get_User_Identifier);
      AWA.Questions.Models.List (List, Session, Query);
      if List.List.Is_Empty then
         Outcome := Ada.Strings.Unbounded.To_Unbounded_String ("not-found");
         return;
      end if;
      Bean.Question := List.List.Element (1);

      Query.Clear;
      Query.Bind_Param ("question_id", Bean.Id);
      Query.Bind_Param ("user_id", Ctx.Get_User_Identifier);
      Query.Set_Query (AWA.Questions.Models.Query_Answer_List);
      AWA.Questions.Models.List (Bean.Answer_List, Session, Query);

      --  Load the tags if any.
      Bean.Tags.Load_Tags (Session, Bean.Id);
   end Load;

   --  ------------------------------
   --  Create the Question_Display_Bean bean instance.
   --  ------------------------------
   function Create_Question_Display_Bean (Module : in AWA.Questions.Modules.Question_Module_Access)
                                       return Util.Beans.Basic.Readonly_Bean_Access is
      Object  : constant Question_Display_Bean_Access := new Question_Display_Bean;
   begin
      Object.Service          := Module;
      Object.Question_Bean    := Object.Question'Access;
      Object.Answer_List_Bean := Object.Answer_List'Access;
      Object.Tags_Bean        := Object.Tags'Access;
      Object.Tags.Set_Entity_Type (AWA.Questions.Models.QUESTION_TABLE);
      return Object.all'Access;
   end Create_Question_Display_Bean;

end AWA.Questions.Beans;
