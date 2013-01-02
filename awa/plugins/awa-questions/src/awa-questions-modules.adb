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

with AWA.Modules.Beans;
with AWA.Modules.Get;
with Util.Log.loggers;
with AWA.Questions.Beans;
package body AWA.Questions.Modules is

   Log : constant Util.log.Loggers.Logger := Util.Log.Loggers.Create ("AWA.Questions.Module");

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

      --  Register here any bean class, servlet, filter.
      Register.Register (Plugin => Plugin,
                         Name   => "AWA.Questions.Beans.Questions_Bean",
                         Handler => AWA.Questions.Beans.Create_Question_Bean'Access);

      AWA.Modules.Module (Plugin).Initialize (App, Props);

      --  Add here the creation of manager instances.
   end Initialize;

   --  ------------------------------
   --  Get the questions module.
   --  ------------------------------
   function Get_Question_Module return Question_Module_Access is
      function Get is new AWA.Modules.Get (Question_Module, Question_Module_Access, NAME);
   begin
      return Get;
   end Get_Question_Module;

end AWA.Questions.Modules;
