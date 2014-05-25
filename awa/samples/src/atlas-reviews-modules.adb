-----------------------------------------------------------------------
--  atlas-reviews-modules -- Module reviews
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
with Ada.Calendar;
with AWA.Modules.Beans;
with AWA.Modules.Get;
with Util.Log.Loggers;
with Atlas.Reviews.Beans;
with ADO.Sessions;
with AWA.Services.Contexts;
with AWA.Permissions;
package body Atlas.Reviews.Modules is

   package ASC renames AWA.Services.Contexts;

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Atlas.Reviews.Module");

   package Register is new AWA.Modules.Beans (Module => Review_Module,
                                              Module_Access => Review_Module_Access);

   --  ------------------------------
   --  Initialize the reviews module.
   --  ------------------------------
   overriding
   procedure Initialize (Plugin : in out Review_Module;
                         App    : in AWA.Modules.Application_Access;
                         Props  : in ASF.Applications.Config) is
   begin
      Log.Info ("Initializing the reviews module");

      --  Register here any bean class, servlet, filter.
      Register.Register (Plugin => Plugin,
                         Name   => "Atlas.Reviews.Beans.Reviews_Bean",
                         Handler => Atlas.Reviews.Beans.Create_Review_Bean'Access);
      Register.Register (Plugin => Plugin,
                         Name   => "Atlas.Reviews.Beans.Review_List_Bean",
                         Handler => Atlas.Reviews.Beans.Create_Review_List_Bean'Access);

      AWA.Modules.Module (Plugin).Initialize (App, Props);

      --  Add here the creation of manager instances.
   end Initialize;

   --  ------------------------------
   --  Get the reviews module.
   --  ------------------------------
   function Get_Review_Module return Review_Module_Access is
      function Get is new AWA.Modules.Get (Review_Module, Review_Module_Access, NAME);
   begin
      return Get;
   end Get_Review_Module;


   --  ------------------------------
   --  Save the review.
   --  ------------------------------
   procedure Save (Model  : in Review_Module;
                   Entity : in out Atlas.Reviews.Models.Review_Ref'Class) is
      pragma Unreferenced (Model);

      Ctx   : constant ASC.Service_Context_Access := ASC.Current;
      DB    : ADO.Sessions.Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
   begin
      Ctx.Start;
      if not Entity.Is_Inserted then
         AWA.Permissions.Check (Permission => ACL_Create_Reviews.Permission);
         Entity.Set_Reviewer (Ctx.Get_User);
         Entity.Set_Create_Date (Ada.Calendar.Clock);
      else
         AWA.Permissions.Check (Permission => ACL_Update_Reviews.Permission,
                                Entity     => Entity);
      end if;
      Entity.Save (DB);
      Ctx.Commit;
   end Save;

   --  ------------------------------
   --  Delete the review.
   --  ------------------------------
   procedure Delete (Model  : in Review_Module;
                     Entity : in out Atlas.Reviews.Models.Review_Ref'Class) is
      pragma Unreferenced (Model);

      Ctx   : constant ASC.Service_Context_Access := ASC.Current;
      DB    : ADO.Sessions.Master_Session := ASC.Get_Master_Session (Ctx);
   begin
      AWA.Permissions.Check (Permission => ACL_Delete_Reviews.Permission,
                             Entity     => Entity);
      Ctx.Start;
      Entity.Delete (DB);
      Ctx.Commit;
   end Delete;

end Atlas.Reviews.Modules;
