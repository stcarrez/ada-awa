-----------------------------------------------------------------------
--  AWA.Questions.Models -- AWA.Questions.Models
-----------------------------------------------------------------------
--  File generated by Dynamo DO NOT MODIFY
--  Template used: templates/model/package-spec.xhtml
--  Ada Generator: https://github.com/stcarrez/dynamo Version 1.2.3
-----------------------------------------------------------------------
--  Copyright (C) 2022 Stephane Carrez
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
pragma Warnings (Off);
with ADO.Sessions;
with ADO.Objects;
with ADO.Statements;
with ADO.SQL;
with ADO.Schemas;
with ADO.Queries;
with ADO.Queries.Loaders;
with Ada.Calendar;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Util.Beans.Objects;
with Util.Beans.Basic.Lists;
with AWA.Users.Models;
with AWA.Workspaces.Models;
with Util.Beans.Methods;
pragma Warnings (On);
package AWA.Questions.Models is

   pragma Style_Checks ("-mr");

   type Question_Ref is new ADO.Objects.Object_Ref with null record;

   type Answer_Ref is new ADO.Objects.Object_Ref with null record;

   --  --------------------
   --  The question table holds a single question asked by a user to the community.
   --  The short description is used to give an overview of the question in long lists
   --  while the description contains the full question text.  The rating is updating
   --  according to users voting for the question.
   --  --------------------
   --  Create an object key for Question.
   function Question_Key (Id : in ADO.Identifier) return ADO.Objects.Object_Key;
   --  Create an object key for Question from a string.
   --  Raises Constraint_Error if the string cannot be converted into the object key.
   function Question_Key (Id : in String) return ADO.Objects.Object_Key;

   Null_Question : constant Question_Ref;
   function "=" (Left, Right : Question_Ref'Class) return Boolean;

   --  Set the date when the question was created.
   procedure Set_Create_Date (Object : in out Question_Ref;
                              Value  : in Ada.Calendar.Time);

   --  Get the date when the question was created.
   function Get_Create_Date (Object : in Question_Ref)
                 return Ada.Calendar.Time;

   --  Set the question title.
   procedure Set_Title (Object : in out Question_Ref;
                        Value  : in Ada.Strings.Unbounded.Unbounded_String);
   procedure Set_Title (Object : in out Question_Ref;
                        Value : in String);

   --  Get the question title.
   function Get_Title (Object : in Question_Ref)
                 return Ada.Strings.Unbounded.Unbounded_String;
   function Get_Title (Object : in Question_Ref)
                 return String;

   --  Set the full description.
   procedure Set_Description (Object : in out Question_Ref;
                              Value  : in Ada.Strings.Unbounded.Unbounded_String);
   procedure Set_Description (Object : in out Question_Ref;
                              Value : in String);

   --  Get the full description.
   function Get_Description (Object : in Question_Ref)
                 return Ada.Strings.Unbounded.Unbounded_String;
   function Get_Description (Object : in Question_Ref)
                 return String;

   --  Set the date when the question was edited.
   procedure Set_Edit_Date (Object : in out Question_Ref;
                            Value  : in ADO.Nullable_Time);

   --  Get the date when the question was edited.
   function Get_Edit_Date (Object : in Question_Ref)
                 return ADO.Nullable_Time;

   --  Set Title: Questions and Answers model
   --  Date: 2015-11-15
   --  the question short description.
   procedure Set_Short_Description (Object : in out Question_Ref;
                                    Value  : in Ada.Strings.Unbounded.Unbounded_String);
   procedure Set_Short_Description (Object : in out Question_Ref;
                                    Value : in String);

   --  Get Title: Questions and Answers model
   --  Date: 2015-11-15
   --  the question short description.
   function Get_Short_Description (Object : in Question_Ref)
                 return Ada.Strings.Unbounded.Unbounded_String;
   function Get_Short_Description (Object : in Question_Ref)
                 return String;

   --  Set the question rating.
   procedure Set_Rating (Object : in out Question_Ref;
                         Value  : in Integer);

   --  Get the question rating.
   function Get_Rating (Object : in Question_Ref)
                 return Integer;

   --  Set the question identifier.
   procedure Set_Id (Object : in out Question_Ref;
                     Value  : in ADO.Identifier);

   --  Get the question identifier.
   function Get_Id (Object : in Question_Ref)
                 return ADO.Identifier;
   --  Get the optimistic locking version.
   function Get_Version (Object : in Question_Ref)
                 return Integer;

   --  Set the user who asked the question.
   procedure Set_Author (Object : in out Question_Ref;
                         Value  : in AWA.Users.Models.User_Ref'Class);

   --  Get the user who asked the question.
   function Get_Author (Object : in Question_Ref)
                 return AWA.Users.Models.User_Ref'Class;

   --
   procedure Set_Workspace (Object : in out Question_Ref;
                            Value  : in AWA.Workspaces.Models.Workspace_Ref'Class);

   --
   function Get_Workspace (Object : in Question_Ref)
                 return AWA.Workspaces.Models.Workspace_Ref'Class;

   --
   procedure Set_Accepted_Answer (Object : in out Question_Ref;
                                  Value  : in Answer_Ref'Class);

   --
   function Get_Accepted_Answer (Object : in Question_Ref)
                 return Answer_Ref'Class;

   --  Load the entity identified by 'Id'.
   --  Raises the NOT_FOUND exception if it does not exist.
   procedure Load (Object  : in out Question_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier);

   --  Load the entity identified by 'Id'.
   --  Returns True in <b>Found</b> if the object was found and False if it does not exist.
   procedure Load (Object  : in out Question_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier;
                   Found   : out Boolean);

   --  Reload from the database the same object if it was modified.
   --  Returns True in `Updated` if the object was reloaded.
   --  Raises the NOT_FOUND exception if it does not exist.
   procedure Reload (Object  : in out Question_Ref;
                     Session : in out ADO.Sessions.Session'Class;
                     Updated : out Boolean);

   --  Find and load the entity.
   overriding
   procedure Find (Object  : in out Question_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   --  Save the entity.  If the entity does not have an identifier, an identifier is allocated
   --  and it is inserted in the table.  Otherwise, only data fields which have been changed
   --  are updated.
   overriding
   procedure Save (Object  : in out Question_Ref;
                   Session : in out ADO.Sessions.Master_Session'Class);

   --  Delete the entity.
   overriding
   procedure Delete (Object  : in out Question_Ref;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   function Get_Value (From : in Question_Ref;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Table definition
   QUESTION_TABLE : constant ADO.Schemas.Class_Mapping_Access;

   --  Internal method to allocate the Object_Record instance
   overriding
   procedure Allocate (Object : in out Question_Ref);

   --  Copy of the object.
   procedure Copy (Object : in Question_Ref;
                   Into   : in out Question_Ref);

   --  --------------------
   --  The answer table gives a list of anwsers to the question.
   --  Ranking is updating according to users voting for the anwser.
   --  --------------------
   --  Create an object key for Answer.
   function Answer_Key (Id : in ADO.Identifier) return ADO.Objects.Object_Key;
   --  Create an object key for Answer from a string.
   --  Raises Constraint_Error if the string cannot be converted into the object key.
   function Answer_Key (Id : in String) return ADO.Objects.Object_Key;

   Null_Answer : constant Answer_Ref;
   function "=" (Left, Right : Answer_Ref'Class) return Boolean;

   --  Set the answer creation date.
   procedure Set_Create_Date (Object : in out Answer_Ref;
                              Value  : in Ada.Calendar.Time);

   --  Get the answer creation date.
   function Get_Create_Date (Object : in Answer_Ref)
                 return Ada.Calendar.Time;

   --  Set the date when the answer was edited.
   procedure Set_Edit_Date (Object : in out Answer_Ref;
                            Value  : in ADO.Nullable_Time);

   --  Get the date when the answer was edited.
   function Get_Edit_Date (Object : in Answer_Ref)
                 return ADO.Nullable_Time;

   --  Set the answer text.
   procedure Set_Answer (Object : in out Answer_Ref;
                         Value  : in Ada.Strings.Unbounded.Unbounded_String);
   procedure Set_Answer (Object : in out Answer_Ref;
                         Value : in String);

   --  Get the answer text.
   function Get_Answer (Object : in Answer_Ref)
                 return Ada.Strings.Unbounded.Unbounded_String;
   function Get_Answer (Object : in Answer_Ref)
                 return String;

   --  Set the anwser rank number.
   procedure Set_Rank (Object : in out Answer_Ref;
                       Value  : in Integer);

   --  Get the anwser rank number.
   function Get_Rank (Object : in Answer_Ref)
                 return Integer;

   --  Set the answer identifier.
   procedure Set_Id (Object : in out Answer_Ref;
                     Value  : in ADO.Identifier);

   --  Get the answer identifier.
   function Get_Id (Object : in Answer_Ref)
                 return ADO.Identifier;
   --
   function Get_Version (Object : in Answer_Ref)
                 return Integer;

   --  Set the user who wrote the answer.
   procedure Set_Author (Object : in out Answer_Ref;
                         Value  : in AWA.Users.Models.User_Ref'Class);

   --  Get the user who wrote the answer.
   function Get_Author (Object : in Answer_Ref)
                 return AWA.Users.Models.User_Ref'Class;

   --
   procedure Set_Question (Object : in out Answer_Ref;
                           Value  : in Question_Ref'Class);

   --
   function Get_Question (Object : in Answer_Ref)
                 return Question_Ref'Class;

   --  Load the entity identified by 'Id'.
   --  Raises the NOT_FOUND exception if it does not exist.
   procedure Load (Object  : in out Answer_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier);

   --  Load the entity identified by 'Id'.
   --  Returns True in <b>Found</b> if the object was found and False if it does not exist.
   procedure Load (Object  : in out Answer_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier;
                   Found   : out Boolean);

   --  Reload from the database the same object if it was modified.
   --  Returns True in `Updated` if the object was reloaded.
   --  Raises the NOT_FOUND exception if it does not exist.
   procedure Reload (Object  : in out Answer_Ref;
                     Session : in out ADO.Sessions.Session'Class;
                     Updated : out Boolean);

   --  Find and load the entity.
   overriding
   procedure Find (Object  : in out Answer_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   --  Save the entity.  If the entity does not have an identifier, an identifier is allocated
   --  and it is inserted in the table.  Otherwise, only data fields which have been changed
   --  are updated.
   overriding
   procedure Save (Object  : in out Answer_Ref;
                   Session : in out ADO.Sessions.Master_Session'Class);

   --  Delete the entity.
   overriding
   procedure Delete (Object  : in out Answer_Ref;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   function Get_Value (From : in Answer_Ref;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Table definition
   ANSWER_TABLE : constant ADO.Schemas.Class_Mapping_Access;

   --  Internal method to allocate the Object_Record instance
   overriding
   procedure Allocate (Object : in out Answer_Ref);

   --  Copy of the object.
   procedure Copy (Object : in Answer_Ref;
                   Into   : in out Answer_Ref);


   --  --------------------
   --    The list of answers.
   --  --------------------
   type Answer_Info is
     new Util.Beans.Basic.Bean with  record

      --  the answer identifier.
      Id : ADO.Identifier;

      --  the answer creation date.
      Create_Date : Ada.Calendar.Time;

      --  the answer edit date.
      Edit_Date : ADO.Nullable_Time;

      --  the answer description.
      Answer : Ada.Strings.Unbounded.Unbounded_String;

      --  the answer rank.
      Rank : Integer;

      --  the question rating as voted by the current user.
      User_Rating : Integer;

      --  the author's identifier.
      Author_Id : ADO.Identifier;

      --  the author's name.
      Author_Name : Ada.Strings.Unbounded.Unbounded_String;

      --  the author's email.
      Author_Email : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   --  Get the bean attribute identified by the name.
   overriding
   function Get_Value (From : in Answer_Info;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the bean attribute identified by the name.
   overriding
   procedure Set_Value (Item  : in out Answer_Info;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);


   package Answer_Info_Beans is
      new Util.Beans.Basic.Lists (Element_Type => Answer_Info);
   package Answer_Info_Vectors renames Answer_Info_Beans.Vectors;
   subtype Answer_Info_List_Bean is Answer_Info_Beans.List_Bean;

   type Answer_Info_List_Bean_Access is access all Answer_Info_List_Bean;

   --  Run the query controlled by <b>Context</b> and append the list in <b>Object</b>.
   procedure List (Object  : in out Answer_Info_List_Bean'Class;
                   Session : in out ADO.Sessions.Session'Class;
                   Context : in out ADO.Queries.Context'Class);

   subtype Answer_Info_Vector is Answer_Info_Vectors.Vector;

   --  Run the query controlled by <b>Context</b> and append the list in <b>Object</b>.
   procedure List (Object  : in out Answer_Info_Vector;
                   Session : in out ADO.Sessions.Session'Class;
                   Context : in out ADO.Queries.Context'Class);

   Query_Answer_List : constant ADO.Queries.Query_Definition_Access;

   --  --------------------
   --    The list of questions.
   --  --------------------
   type Question_Display_Info is
     new Util.Beans.Basic.Bean with  record

      --  the question identifier.
      Id : ADO.Identifier;

      --  the question title.
      Title : Ada.Strings.Unbounded.Unbounded_String;

      --  the question creation date.
      Create_Date : Ada.Calendar.Time;

      --  the question edit date.
      Edit_Date : ADO.Nullable_Time;

      --  the question description.
      Description : Ada.Strings.Unbounded.Unbounded_String;

      --  the question rating.
      Rating : Integer;

      --  the question rating as voted by the current user.
      User_Rating : Integer;

      --  the author's identifier.
      Author_Id : ADO.Identifier;

      --  the author's name.
      Author_Name : Ada.Strings.Unbounded.Unbounded_String;

      --  the author's email.
      Author_Email : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   --  Get the bean attribute identified by the name.
   overriding
   function Get_Value (From : in Question_Display_Info;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the bean attribute identified by the name.
   overriding
   procedure Set_Value (Item  : in out Question_Display_Info;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);


   package Question_Display_Info_Beans is
      new Util.Beans.Basic.Lists (Element_Type => Question_Display_Info);
   package Question_Display_Info_Vectors renames Question_Display_Info_Beans.Vectors;
   subtype Question_Display_Info_List_Bean is Question_Display_Info_Beans.List_Bean;

   type Question_Display_Info_List_Bean_Access is access all Question_Display_Info_List_Bean;

   --  Run the query controlled by <b>Context</b> and append the list in <b>Object</b>.
   procedure List (Object  : in out Question_Display_Info_List_Bean'Class;
                   Session : in out ADO.Sessions.Session'Class;
                   Context : in out ADO.Queries.Context'Class);

   subtype Question_Display_Info_Vector is Question_Display_Info_Vectors.Vector;

   --  Run the query controlled by <b>Context</b> and append the list in <b>Object</b>.
   procedure List (Object  : in out Question_Display_Info_Vector;
                   Session : in out ADO.Sessions.Session'Class;
                   Context : in out ADO.Queries.Context'Class);

   Query_Question_Info : constant ADO.Queries.Query_Definition_Access;

   --  --------------------
   --    The list of questions.
   --  --------------------
   type Question_Info is
     new Util.Beans.Basic.Bean with  record

      --  the question identifier.
      Id : ADO.Identifier;

      --  the question title.
      Title : Ada.Strings.Unbounded.Unbounded_String;

      --  the question creation date.
      Create_Date : Ada.Calendar.Time;

      --  the question short description.
      Description : Ada.Strings.Unbounded.Unbounded_String;

      --  the question rating.
      Rating : Integer;

      --  the number of answers.
      Answer_Count : Integer;

      --  the author's identifier.
      Author_Id : ADO.Identifier;

      --  the author's name.
      Author_Name : Ada.Strings.Unbounded.Unbounded_String;

      --  the author's email.
      Author_Email : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   --  Get the bean attribute identified by the name.
   overriding
   function Get_Value (From : in Question_Info;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the bean attribute identified by the name.
   overriding
   procedure Set_Value (Item  : in out Question_Info;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);


   package Question_Info_Beans is
      new Util.Beans.Basic.Lists (Element_Type => Question_Info);
   package Question_Info_Vectors renames Question_Info_Beans.Vectors;
   subtype Question_Info_List_Bean is Question_Info_Beans.List_Bean;

   type Question_Info_List_Bean_Access is access all Question_Info_List_Bean;

   --  Run the query controlled by <b>Context</b> and append the list in <b>Object</b>.
   procedure List (Object  : in out Question_Info_List_Bean'Class;
                   Session : in out ADO.Sessions.Session'Class;
                   Context : in out ADO.Queries.Context'Class);

   subtype Question_Info_Vector is Question_Info_Vectors.Vector;

   --  Run the query controlled by <b>Context</b> and append the list in <b>Object</b>.
   procedure List (Object  : in out Question_Info_Vector;
                   Session : in out ADO.Sessions.Session'Class;
                   Context : in out ADO.Queries.Context'Class);

   Query_Question_List : constant ADO.Queries.Query_Definition_Access;

   Query_Question_Tag_List : constant ADO.Queries.Query_Definition_Access;


   type Question_Bean is abstract new AWA.Questions.Models.Question_Ref
     and Util.Beans.Basic.Bean and Util.Beans.Methods.Method_Bean with null record;


   --  This bean provides some methods that can be used in a Method_Expression.
   overriding
   function Get_Method_Bindings (From : in Question_Bean)
                                 return Util.Beans.Methods.Method_Binding_Array_Access;


   --  Set the bean attribute identified by the name.
   overriding
   procedure Set_Value (Item  : in out Question_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   procedure Save (Bean : in out Question_Bean;
                  Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is abstract;

   procedure Delete (Bean : in out Question_Bean;
                    Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is abstract;

   procedure Load (Bean : in out Question_Bean;
                  Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is abstract;

   type Answer_Bean is abstract new AWA.Questions.Models.Answer_Ref
     and Util.Beans.Basic.Bean and Util.Beans.Methods.Method_Bean with null record;


   --  This bean provides some methods that can be used in a Method_Expression.
   overriding
   function Get_Method_Bindings (From : in Answer_Bean)
                                 return Util.Beans.Methods.Method_Binding_Array_Access;


   --  Set the bean attribute identified by the name.
   overriding
   procedure Set_Value (Item  : in out Answer_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   procedure Save (Bean : in out Answer_Bean;
                  Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is abstract;

   procedure Delete (Bean : in out Answer_Bean;
                    Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is abstract;

   procedure Load (Bean : in out Answer_Bean;
                  Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is abstract;

   --  --------------------
   --    load the list of questions
   --  --------------------
   type Question_List_Bean is abstract limited
     new Util.Beans.Basic.Bean and Util.Beans.Methods.Method_Bean with  record

      --  the optional tag to filter questions.
      Tag : Ada.Strings.Unbounded.Unbounded_String;
      Page : Integer;

      --  the number of questions per page.
      Page_Size : Integer;

      --  the number of questions.
      Count : Integer;
   end record;

   --  This bean provides some methods that can be used in a Method_Expression.
   overriding
   function Get_Method_Bindings (From : in Question_List_Bean)
                                 return Util.Beans.Methods.Method_Binding_Array_Access;

   --  Get the bean attribute identified by the name.
   overriding
   function Get_Value (From : in Question_List_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the bean attribute identified by the name.
   overriding
   procedure Set_Value (Item  : in out Question_List_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   procedure Load (Bean : in out Question_List_Bean;
                  Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is abstract;

   type Question_Display_Bean is abstract limited
     new Util.Beans.Basic.Bean and Util.Beans.Methods.Method_Bean with null record;


   --  This bean provides some methods that can be used in a Method_Expression.
   overriding
   function Get_Method_Bindings (From : in Question_Display_Bean)
                                 return Util.Beans.Methods.Method_Binding_Array_Access;

   --  Get the bean attribute identified by the name.
   overriding
   function Get_Value (From : in Question_Display_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the bean attribute identified by the name.
   overriding
   procedure Set_Value (Item  : in out Question_Display_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   procedure Load (Bean : in out Question_Display_Bean;
                  Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is abstract;


private
   QUESTION_NAME : aliased constant String := "awa_question";
   COL_0_1_NAME : aliased constant String := "create_date";
   COL_1_1_NAME : aliased constant String := "title";
   COL_2_1_NAME : aliased constant String := "description";
   COL_3_1_NAME : aliased constant String := "edit_date";
   COL_4_1_NAME : aliased constant String := "short_description";
   COL_5_1_NAME : aliased constant String := "rating";
   COL_6_1_NAME : aliased constant String := "id";
   COL_7_1_NAME : aliased constant String := "version";
   COL_8_1_NAME : aliased constant String := "author_id";
   COL_9_1_NAME : aliased constant String := "workspace_id";
   COL_10_1_NAME : aliased constant String := "accepted_answer_id";

   QUESTION_DEF : aliased constant ADO.Schemas.Class_Mapping :=
     (Count   => 11,
      Table   => QUESTION_NAME'Access,
      Members => (
         1 => COL_0_1_NAME'Access,
         2 => COL_1_1_NAME'Access,
         3 => COL_2_1_NAME'Access,
         4 => COL_3_1_NAME'Access,
         5 => COL_4_1_NAME'Access,
         6 => COL_5_1_NAME'Access,
         7 => COL_6_1_NAME'Access,
         8 => COL_7_1_NAME'Access,
         9 => COL_8_1_NAME'Access,
         10 => COL_9_1_NAME'Access,
         11 => COL_10_1_NAME'Access)
     );
   QUESTION_TABLE : constant ADO.Schemas.Class_Mapping_Access
      := QUESTION_DEF'Access;


   Null_Question : constant Question_Ref
      := Question_Ref'(ADO.Objects.Object_Ref with null record);

   type Question_Impl is
      new ADO.Objects.Object_Record (Key_Type => ADO.Objects.KEY_INTEGER,
                                     Of_Class => QUESTION_DEF'Access)
   with record
       Create_Date : Ada.Calendar.Time;
       Title : Ada.Strings.Unbounded.Unbounded_String;
       Description : Ada.Strings.Unbounded.Unbounded_String;
       Edit_Date : ADO.Nullable_Time;
       Short_Description : Ada.Strings.Unbounded.Unbounded_String;
       Rating : Integer;
       Version : Integer;
       Author : AWA.Users.Models.User_Ref;
       Workspace : AWA.Workspaces.Models.Workspace_Ref;
       Accepted_Answer : Answer_Ref;
   end record;

   type Question_Access is access all Question_Impl;

   overriding
   procedure Destroy (Object : access Question_Impl);

   overriding
   procedure Find (Object  : in out Question_Impl;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   overriding
   procedure Load (Object  : in out Question_Impl;
                   Session : in out ADO.Sessions.Session'Class);
   procedure Load (Object  : in out Question_Impl;
                   Stmt    : in out ADO.Statements.Query_Statement'Class;
                   Session : in out ADO.Sessions.Session'Class);

   overriding
   procedure Save (Object  : in out Question_Impl;
                   Session : in out ADO.Sessions.Master_Session'Class);

   procedure Create (Object  : in out Question_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   procedure Delete (Object  : in out Question_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   procedure Set_Field (Object : in out Question_Ref'Class;
                        Impl   : out Question_Access);
   ANSWER_NAME : aliased constant String := "awa_answer";
   COL_0_2_NAME : aliased constant String := "create_date";
   COL_1_2_NAME : aliased constant String := "edit_date";
   COL_2_2_NAME : aliased constant String := "answer";
   COL_3_2_NAME : aliased constant String := "rank";
   COL_4_2_NAME : aliased constant String := "id";
   COL_5_2_NAME : aliased constant String := "version";
   COL_6_2_NAME : aliased constant String := "author_id";
   COL_7_2_NAME : aliased constant String := "question_id";

   ANSWER_DEF : aliased constant ADO.Schemas.Class_Mapping :=
     (Count   => 8,
      Table   => ANSWER_NAME'Access,
      Members => (
         1 => COL_0_2_NAME'Access,
         2 => COL_1_2_NAME'Access,
         3 => COL_2_2_NAME'Access,
         4 => COL_3_2_NAME'Access,
         5 => COL_4_2_NAME'Access,
         6 => COL_5_2_NAME'Access,
         7 => COL_6_2_NAME'Access,
         8 => COL_7_2_NAME'Access)
     );
   ANSWER_TABLE : constant ADO.Schemas.Class_Mapping_Access
      := ANSWER_DEF'Access;


   Null_Answer : constant Answer_Ref
      := Answer_Ref'(ADO.Objects.Object_Ref with null record);

   type Answer_Impl is
      new ADO.Objects.Object_Record (Key_Type => ADO.Objects.KEY_INTEGER,
                                     Of_Class => ANSWER_DEF'Access)
   with record
       Create_Date : Ada.Calendar.Time;
       Edit_Date : ADO.Nullable_Time;
       Answer : Ada.Strings.Unbounded.Unbounded_String;
       Rank : Integer;
       Version : Integer;
       Author : AWA.Users.Models.User_Ref;
       Question : Question_Ref;
   end record;

   type Answer_Access is access all Answer_Impl;

   overriding
   procedure Destroy (Object : access Answer_Impl);

   overriding
   procedure Find (Object  : in out Answer_Impl;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   overriding
   procedure Load (Object  : in out Answer_Impl;
                   Session : in out ADO.Sessions.Session'Class);
   procedure Load (Object  : in out Answer_Impl;
                   Stmt    : in out ADO.Statements.Query_Statement'Class;
                   Session : in out ADO.Sessions.Session'Class);

   overriding
   procedure Save (Object  : in out Answer_Impl;
                   Session : in out ADO.Sessions.Master_Session'Class);

   procedure Create (Object  : in out Answer_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   procedure Delete (Object  : in out Answer_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   procedure Set_Field (Object : in out Answer_Ref'Class;
                        Impl   : out Answer_Access);

   package File_1 is
      new ADO.Queries.Loaders.File (Path => "answer-list.xml",
                                    Sha1 => "2EDE0E19B5963DDEE7C4F8CE3A1B331A0063EC0A");

   package Def_Answerinfo_Answer_List is
      new ADO.Queries.Loaders.Query (Name => "answer-list",
                                     File => File_1.File'Access);
   Query_Answer_List : constant ADO.Queries.Query_Definition_Access
   := Def_Answerinfo_Answer_List.Query'Access;

   package File_2 is
      new ADO.Queries.Loaders.File (Path => "question-info.xml",
                                    Sha1 => "EEBB96CEACAFAD6A3C4CBFCE81E28E885E0740A2");

   package Def_Questiondisplayinfo_Question_Info is
      new ADO.Queries.Loaders.Query (Name => "question-info",
                                     File => File_2.File'Access);
   Query_Question_Info : constant ADO.Queries.Query_Definition_Access
   := Def_Questiondisplayinfo_Question_Info.Query'Access;

   package File_3 is
      new ADO.Queries.Loaders.File (Path => "question-list.xml",
                                    Sha1 => "6B1373D779DD15CEB92310B13BBB715BD674231C");

   package Def_Questioninfo_Question_List is
      new ADO.Queries.Loaders.Query (Name => "question-list",
                                     File => File_3.File'Access);
   Query_Question_List : constant ADO.Queries.Query_Definition_Access
   := Def_Questioninfo_Question_List.Query'Access;

   package Def_Questioninfo_Question_Tag_List is
      new ADO.Queries.Loaders.Query (Name => "question-tag-list",
                                     File => File_3.File'Access);
   Query_Question_Tag_List : constant ADO.Queries.Query_Definition_Access
   := Def_Questioninfo_Question_Tag_List.Query'Access;
end AWA.Questions.Models;
