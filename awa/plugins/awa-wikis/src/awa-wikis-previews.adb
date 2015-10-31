-----------------------------------------------------------------------
--  awa-wikis-previews -- Wiki preview management
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
with Util.Log.Loggers;

with AWA.Jobs.Services;
with AWA.Jobs.Modules;
package body AWA.Wikis.Previews is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("AWA.Wikis.Preview");

   --  ------------------------------
   --  The worker procedure that performs the preview job.
   --  ------------------------------
   procedure Preview_Worker (Job : in out AWA.Jobs.Services.Abstract_Job_Type'Class) is
   begin
      null;
   end Preview_Worker;

   --  ------------------------------
   --  Initialize the wikis module.
   --  ------------------------------
   overriding
   procedure Initialize (Plugin : in out Preview_Module;
                         App    : in AWA.Modules.Application_Access;
                         Props  : in ASF.Applications.Config) is
   begin
      Log.Info ("Initializing the wiki preview module");

      AWA.Modules.Module (Plugin).Initialize (App, Props);
      Plugin.Add_Listener (AWA.Wikis.Modules.NAME, Plugin'Unchecked_Access);
      Plugin.Job_Module := AWA.Jobs.Modules.Get_Job_Module;
      Plugin.Job_Module.Register (Definition => Preview_Job_Definition.Factory);
   end Initialize;

   --  ------------------------------
   --  Create a preview job and schedule the job to generate a new thumbnail preview for the page.
   --  ------------------------------
   procedure Make_Preview_Job (Plugin : in Preview_Module;
                               Page   : in AWA.Wikis.Models.Wiki_Page_Ref'Class) is
      J : AWA.Jobs.Services.Job_Type;
   begin
      J.Set_Parameter ("wiki_space_id", Page.Get_Wiki);
      J.Set_Parameter ("wiki_page_id", Page);
      J.Schedule (Preview_Job_Definition.Factory.all);
   end Make_Preview_Job;

   --  ------------------------------
   --  The `On_Create` procedure is called by `Notify_Create` to notify the creation of the page.
   --  ------------------------------
   overriding
   procedure On_Create (Instance : in Preview_Module;
                        Item     : in AWA.Wikis.Models.Wiki_Page_Ref'Class) is
   begin
      null;
   end On_Create;

   --  ------------------------------
   --  The `On_Update` procedure is called by `Notify_Update` to notify the update of the page.
   --  ------------------------------
   overriding
   procedure On_Update (Instance : in Preview_Module;
                        Item     : in AWA.Wikis.Models.Wiki_Page_Ref'Class) is
   begin
      null;
   end On_Update;

   --  ------------------------------
   --  The `On_Delete` procedure is called by `Notify_Delete` to notify the deletion of the page.
   --  ------------------------------
   overriding
   procedure On_Delete (Instance : in Preview_Module;
                        Item     : in AWA.Wikis.Models.Wiki_Page_Ref'Class) is
   begin
      null;
   end On_Delete;

end AWA.Wikis.Previews;
