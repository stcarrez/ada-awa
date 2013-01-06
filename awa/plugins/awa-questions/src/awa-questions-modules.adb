-----------------------------------------------------------------------
--  awa-questions-modules -- Module questions
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
with Util.Log.Loggers;

with AWA.Modules.Beans;
with AWA.Modules.Get;
with AWA.Questions.Beans;
with AWA.Applications;
package body AWA.Questions.Modules is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("AWA.Questions.Module");

   package Register is new AWA.Modules.Beans (Module => Question_Module,
                                              Module_Access => Question_Module_Access);

   --  ------------------------------
   --  Initialize the questions module.
   --  ------------------------------
   overriding
   procedure Initialize (Plugin : in out Question_Module;
                         App    : in AWA.Modules.Application_Access;
                         Props  : in ASF.Applications.Config) is
   begin
      Log.Info ("Initializing the questions module");

      --  Setup the resource bundles.
      App.Register ("questionMsg", "questions");

      --  Edit and save a question.
      Register.Register (Plugin => Plugin,
                         Name   =>  "AWA.Questions.Beans.Question_Bean",
                         Handler => AWA.Questions.Beans.Create_Question_Bean'Access);

      --  Edit and save an answer.
      Register.Register (Plugin => Plugin,
                         Name   =>  "AWA.Questions.Beans.Answer_Bean",
                         Handler => AWA.Questions.Beans.Create_Answer_Bean'Access);

      --  List of questions.
      Register.Register (Plugin => Plugin,
                         Name   => "AWA.Questions.Beans.Question_List_Bean",
                         Handler => AWA.Questions.Beans.Create_Question_List_Bean'Access);

      --  Display a question with its answers.
      Register.Register (Plugin => Plugin,
                         Name   => "AWA.Questions.Beans.Question_Display_Bean",
                         Handler => AWA.Questions.Beans.Create_Question_Display_Bean'Access);

      AWA.Modules.Module (Plugin).Initialize (App, Props);

      --  Add here the creation of manager instances.
      Plugin.Manager := Plugin.Create_Question_Manager;
   end Initialize;

   --  ------------------------------
   --  Get the question manager.
   --  ------------------------------
   function Get_Question_Manager (Plugin : in Question_Module)
                                  return Services.Question_Service_Access is
   begin
      return Plugin.Manager;
   end Get_Question_Manager;

   --  ------------------------------
   --  Create a question manager.  This operation can be overridden to provide another
   --  question service implementation.
   --  ------------------------------
   function Create_Question_Manager (Plugin : in Question_Module)
                                     return Services.Question_Service_Access is
      Result : constant Services.Question_Service_Access := new Services.Question_Service;
   begin
      Result.Initialize (Plugin);
      return Result;
   end Create_Question_Manager;

   --  ------------------------------
   --  Get the questions module.
   --  ------------------------------
   function Get_Question_Module return Question_Module_Access is
      function Get is new AWA.Modules.Get (Question_Module, Question_Module_Access, NAME);
   begin
      return Get;
   end Get_Question_Module;

   --  ------------------------------
   --  Get the question manager instance associated with the current application.
   --  ------------------------------
   function Get_Question_Manager return Services.Question_Service_Access is
      Module : constant Question_Module_Access := Get_Question_Module;
   begin
      if Module = null then
         Log.Error ("There is no active Question_Module");
         return null;
      else
         return Module.Get_Question_Manager;
      end if;
   end Get_Question_Manager;

end AWA.Questions.Modules;
