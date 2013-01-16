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

with Ada.Strings.Unbounded;

with Util.Beans.Basic;
with Util.Beans.Objects;
with AWA.Questions.Modules;
with AWA.Questions.Models;
package AWA.Questions.Beans is

   type Question_Bean is new AWA.Questions.Models.Question_Bean with record
      Service : Modules.Question_Module_Access := null;
   end record;
   type Question_Bean_Access is access all Question_Bean'Class;

   --  Get the value identified by the name.
   overriding
   function Get_Value (From : in Question_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the value identified by the name.
   overriding
   procedure Set_Value (From  : in out Question_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   --  Create or save the question.
   procedure Save (Bean    : in out Question_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   --  Delete the question.
   procedure Delete (Bean : in out Question_Bean;
                     Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   --  Create the Questions_Bean bean instance.
   function Create_Question_Bean (Module : in AWA.Questions.Modules.Question_Module_Access)
                                  return Util.Beans.Basic.Readonly_Bean_Access;

   type Answer_Bean is new AWA.Questions.Models.Answer_Bean with record
      Service  : Modules.Question_Module_Access := null;
      Question : AWA.Questions.Models.Question_Ref;
   end record;
   type Answer_Bean_Access is access all Answer_Bean'Class;

   --  Get the value identified by the name.
   overriding
   function Get_Value (From : in Answer_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the value identified by the name.
   overriding
   procedure Set_Value (From  : in out Answer_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   --  Create or save the answer.
   procedure Save (Bean    : in out Answer_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   --  Delete the question.
   procedure Delete (Bean    : in out Answer_Bean;
                     Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   --  Create the answer bean instance.
   function Create_Answer_Bean (Module : in AWA.Questions.Modules.Question_Module_Access)
                                       return Util.Beans.Basic.Readonly_Bean_Access;

   --  Create the Question_Info_List_Bean bean instance.
   function Create_Question_List_Bean (Module : in AWA.Questions.Modules.Question_Module_Access)
                                     return Util.Beans.Basic.Readonly_Bean_Access;


   type Question_Display_Bean is new Util.Beans.Basic.Bean with record
      Service          : Modules.Question_Module_Access := null;

      Answer_List      : aliased AWA.Questions.Models.Answer_Info_List_Bean;
      Answer_List_Bean : AWA.Questions.Models.Answer_Info_List_Bean_Access;

      Question         : aliased AWA.Questions.Models.Question_Display_Info;
      Question_Bean    : Util.Beans.Basic.Readonly_Bean_Access;
   end record;
   type Question_Display_Bean_Access is access all Question_Display_Bean'Class;

   --  Get the value identified by the name.
   overriding
   function Get_Value (From : in Question_Display_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the value identified by the name.
   overriding
   procedure Set_Value (From  : in out Question_Display_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   --  Create the Question_Display_Bean bean instance.
   function Create_Question_Display_Bean (Module : in AWA.Questions.Modules.Question_Module_Access)
                                          return Util.Beans.Basic.Readonly_Bean_Access;

end AWA.Questions.Beans;
