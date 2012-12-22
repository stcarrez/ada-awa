-----------------------------------------------------------------------
--  atlas-microblog-modules -- Module microblog
--  Copyright (C) 2012 Stephane Carrez
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
with Ada.Calendar;

with AWA.Modules.Beans;
with AWA.Modules.Get;
with AWA.Services.Contexts;

with ADO.Sessions;

with Util.Log.Loggers;
with Atlas.Microblog.Beans;
package body Atlas.Microblog.Modules is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Atlas.Microblog.Module");

   package Register is new AWA.Modules.Beans (Module => Microblog_Module,
                                              Module_Access => Microblog_Module_Access);

   --  ------------------------------
   --  Initialize the microblog module.
   --  ------------------------------
   overriding
   procedure Initialize (Plugin : in out Microblog_Module;
                         App    : in AWA.Modules.Application_Access;
                         Props  : in ASF.Applications.Config) is
   begin
      Log.Info ("Initializing the microblog module");

      --  Register here any bean class, servlet, filter.
      Register.Register (Plugin => Plugin,
                         Name   => "Atlas.Microblog.Beans.Microblog_Bean",
                         Handler => Atlas.Microblog.Beans.Create_Microblog_Bean'Access);

      Register.Register (Plugin => Plugin,
                         Name   => "Atlas.Microblog.Beans.List_Bean",
                         Handler => Atlas.Microblog.Beans.Create_List_Bean'Access);

      AWA.Modules.Module (Plugin).Initialize (App, Props);

      --  Add here the creation of manager instances.
   end Initialize;

   --  ------------------------------
   --  Get the microblog module.
   --  ------------------------------
   function Get_Microblog_Module return Microblog_Module_Access is
      function Get is new AWA.Modules.Get (Microblog_Module, Microblog_Module_Access, NAME);
   begin
      return Get;
   end Get_Microblog_Module;

   --  ------------------------------
   --  Create a post for the microblog.
   --  ------------------------------
   procedure Create (Plugin : in Microblog_Module;
                     Post   : in out Atlas.Microblog.Models.Mblog_Ref) is
      pragma Unreferenced (Plugin);
      use AWA.Services;

      Ctx   : constant Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      DB    : ADO.Sessions.Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
   begin
      Ctx.Start;
      Post.Set_Creation_Date (Ada.Calendar.Clock);
      Post.Set_Author (Ctx.Get_User);
      Post.Save (DB);
      Ctx.Commit;
   end Create;

end Atlas.Microblog.Modules;
