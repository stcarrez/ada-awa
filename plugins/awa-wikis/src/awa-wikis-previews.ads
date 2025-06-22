-----------------------------------------------------------------------
--  awa-wikis-previews -- Wiki preview management
--  Copyright (C) 2015 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with EL.Expressions;

with ASF.Applications;

with AWA.Modules;
with AWA.Jobs.Services;
with AWA.Jobs.Modules;
with AWA.Wikis.Modules;
with AWA.Wikis.Models;

--  == Wiki Preview Module ==
--  The <tt>AWA.Wikis.Previews</tt> package implements a preview image generation for a wiki page.
--  This module is optional, it is possible to use the wikis without preview support.  When the
--  module is registered, it listens to wiki page lifecycle events.  When a new wiki content is
--  changed, it triggers a job to make the preview.  The preview job uses the
--  <tt>wkhtmotoimage</tt> external program to make the preview image.
package AWA.Wikis.Previews is

   --  The name under which the module is registered.
   NAME : constant String := "wiki_previews";

   --  The configuration parameter that defines how to build the wiki preview template path.
   PARAM_PREVIEW_TEMPLATE : constant String := "wiki_preview_template";

   --  The configuration parameter to build the preview command to execute.
   PARAM_PREVIEW_COMMAND  : constant String := "wiki_preview_command";

   --  The configuration parameter that defines how to build the HTML preview path.
   PARAM_PREVIEW_HTML     : constant String := "wiki_preview_html";

   --  The configuration parameter that defines the path for the tmp directory.
   PARAM_PREVIEW_TMPDIR   : constant String := "wiki_preview_tmp";

   --  The worker procedure that performs the preview job.
   procedure Preview_Worker (Job : in out AWA.Jobs.Services.Abstract_Job_Type'Class);

   --  Preview job definition.
   package Preview_Job_Definition is new AWA.Jobs.Services.Work_Definition (Preview_Worker'Access);

   --  ------------------------------
   --  Preview wiki module
   --  ------------------------------
   type Preview_Module is new AWA.Modules.Module
     and AWA.Wikis.Modules.Wiki_Lifecycle.Listener with private;
   type Preview_Module_Access is access all Preview_Module'Class;

   --  Initialize the preview wiki module.
   overriding
   procedure Initialize (Plugin : in out Preview_Module;
                         App    : in AWA.Modules.Application_Access;
                         Props  : in ASF.Applications.Config);

   --  Configures the module after its initialization and after having read its XML configuration.
   overriding
   procedure Configure (Plugin : in out Preview_Module;
                        Props  : in ASF.Applications.Config);

   --  The `On_Create` procedure is called by `Notify_Create` to notify the creation of the page.
   overriding
   procedure On_Create (Instance : in Preview_Module;
                        Item     : in AWA.Wikis.Models.Wiki_Page_Ref'Class);

   --  The `On_Update` procedure is called by `Notify_Update` to notify the update of the page.
   overriding
   procedure On_Update (Instance : in Preview_Module;
                        Item     : in AWA.Wikis.Models.Wiki_Page_Ref'Class);

   --  The `On_Delete` procedure is called by `Notify_Delete` to notify the deletion of the page.
   overriding
   procedure On_Delete (Instance : in Preview_Module;
                        Item     : in AWA.Wikis.Models.Wiki_Page_Ref'Class);

   --  Create a preview job and schedule the job to generate a new thumbnail preview for the page.
   procedure Make_Preview_Job (Plugin : in Preview_Module;
                               Page   : in AWA.Wikis.Models.Wiki_Page_Ref'Class);

   --  Execute the preview job and make the thumbnail preview.  The page is first rendered in
   --  an HTML text file and the preview is rendered by using an external command.
   procedure Do_Preview_Job (Plugin : in Preview_Module;
                             Job    : in out AWA.Jobs.Services.Abstract_Job_Type'Class);

   --  Get the preview module instance associated with the current application.
   function Get_Preview_Module return Preview_Module_Access;

private

   type Preview_Module is new AWA.Modules.Module
     and AWA.Wikis.Modules.Wiki_Lifecycle.Listener with record
      Template   : EL.Expressions.Expression;
      Command    : EL.Expressions.Expression;
      Html       : EL.Expressions.Expression;
      Job_Module : AWA.Jobs.Modules.Job_Module_Access;
   end record;

end AWA.Wikis.Previews;
