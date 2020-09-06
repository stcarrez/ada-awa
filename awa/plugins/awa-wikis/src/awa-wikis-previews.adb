-----------------------------------------------------------------------
--  awa-wikis-previews -- Wiki preview management
--  Copyright (C) 2015, 2018, 2020 Stephane Carrez
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

with Util.Log.Loggers;
with Util.Beans.Objects;
with Util.Files;
with Util.Processes;
with Util.Streams.Pipes;
with Util.Streams.Texts;

with EL.Contexts.TLS;

with Servlet.Core;
with ASF.Requests.Mockup;
with ASF.Responses.Mockup;

with AWA.Applications;
with AWA.Services.Contexts;
with AWA.Modules.Get;
package body AWA.Wikis.Previews is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("AWA.Wikis.Preview");

   --  ------------------------------
   --  The worker procedure that performs the preview job.
   --  ------------------------------
   procedure Preview_Worker (Job : in out AWA.Jobs.Services.Abstract_Job_Type'Class) is
      Previewer : constant Preview_Module_Access := Get_Preview_Module;
   begin
      Previewer.Do_Preview_Job (Job);
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
   end Initialize;

   --  ------------------------------
   --  Configures the module after its initialization and after having read its XML configuration.
   --  ------------------------------
   overriding
   procedure Configure (Plugin : in out Preview_Module;
                        Props  : in ASF.Applications.Config) is
      pragma Unreferenced (Props);
   begin
      Plugin.Template := Plugin.Get_Config (PARAM_PREVIEW_TEMPLATE);
      Plugin.Command := Plugin.Get_Config (PARAM_PREVIEW_COMMAND);
      Plugin.Html := Plugin.Get_Config (PARAM_PREVIEW_HTML);
      Plugin.Add_Listener (AWA.Wikis.Modules.NAME, Plugin'Unchecked_Access);
      Plugin.Job_Module := AWA.Jobs.Modules.Get_Job_Module;
      Plugin.Job_Module.Register (Definition => Preview_Job_Definition.Factory);
   end Configure;

   --  ------------------------------
   --  Execute the preview job and make the thumbnail preview.  The page is first rendered in
   --  an HTML text file and the preview is rendered by using an external command.
   --  ------------------------------
   procedure Do_Preview_Job (Plugin : in Preview_Module;
                             Job    : in out AWA.Jobs.Services.Abstract_Job_Type'Class) is
      pragma Unreferenced (Job);
      use Util.Beans.Objects;

      Ctx       : constant EL.Contexts.ELContext_Access := EL.Contexts.TLS.Current;
      Template  : constant String := To_String (Plugin.Template.Get_Value (Ctx.all));
      Command   : constant String := To_String (Plugin.Command.Get_Value (Ctx.all));
      Html_File : constant String := To_String (Plugin.Html.Get_Value (Ctx.all));
   begin
      Log.Info ("Preview {0} with {1}", Template, Command);
      declare
         Req       : ASF.Requests.Mockup.Request;
         Reply     : ASF.Responses.Mockup.Response;
         Dispatcher : constant Servlet.Core.Request_Dispatcher
           := Plugin.Get_Application.Get_Request_Dispatcher (Template);
         Result : Ada.Strings.Unbounded.Unbounded_String;
      begin
         Req.Set_Request_URI (Template);
         Req.Set_Method ("GET");

         Servlet.Core.Forward (Dispatcher, Req, Reply);
         Reply.Read_Content (Result);
         Util.Files.Write_File (Html_File, Result);
      end;

      declare
         Pipe    : aliased Util.Streams.Pipes.Pipe_Stream;
         Input   : Util.Streams.Texts.Reader_Stream;
      begin
         Log.Info ("Running preview command {0}", Command);

         Pipe.Open (Command, Util.Processes.READ_ALL);
         Input.Initialize (Pipe'Unchecked_Access, 1024);
         while not Input.Is_Eof loop
            declare
               Line : Ada.Strings.Unbounded.Unbounded_String;
            begin
               Input.Read_Line (Line, False);
               Log.Info ("Received: {0}", Line);
            end;
         end loop;
         Pipe.Close;
         if Pipe.Get_Exit_Status /= 0 then
            Log.Error ("Command {0} exited with status {1}", Command,
                       Integer'Image (Pipe.Get_Exit_Status));
         end if;
      end;
   end Do_Preview_Job;

   --  ------------------------------
   --  Create a preview job and schedule the job to generate a new thumbnail preview for the page.
   --  ------------------------------
   procedure Make_Preview_Job (Plugin : in Preview_Module;
                               Page   : in AWA.Wikis.Models.Wiki_Page_Ref'Class) is
      pragma Unreferenced (Plugin);

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
      Instance.Make_Preview_Job (Item);
   end On_Create;

   --  ------------------------------
   --  The `On_Update` procedure is called by `Notify_Update` to notify the update of the page.
   --  ------------------------------
   overriding
   procedure On_Update (Instance : in Preview_Module;
                        Item     : in AWA.Wikis.Models.Wiki_Page_Ref'Class) is
   begin
      Instance.Make_Preview_Job (Item);
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

   --  ------------------------------
   --  Get the preview module instance associated with the current application.
   --  ------------------------------
   function Get_Preview_Module return Preview_Module_Access is
      function Get is new AWA.Modules.Get (Preview_Module, Preview_Module_Access, NAME);
   begin
      return Get;
   end Get_Preview_Module;

end AWA.Wikis.Previews;
