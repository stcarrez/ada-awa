-----------------------------------------------------------------------
--  awa-wikis-modules -- Module wikis
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
with ASF.Applications;

with ADO;
with AWA.Modules;
with AWA.Wikis.Models;
with Security.Permissions;
with Awa.Wikis.Models;
package AWA.Wikis.Modules is

   --  The name under which the module is registered.
   NAME : constant String := "wikis";

   package ACL_Create_Wiki_Pages is new Security.Permissions.Definition ("wiki-page-create");
   package ACL_Delete_Wiki_Pages is new Security.Permissions.Definition ("wiki-page-delete");
   package ACL_Update_Wiki_Pages is new Security.Permissions.Definition ("wiki-page-update");

   --  Define the permissions.
   package ACL_Create_Wiki_Space is new Security.Permissions.Definition ("wiki-space-create");
   package ACL_Delete_Wiki_Space is new Security.Permissions.Definition ("wiki-space-delete");
   package ACL_Update_Wiki_Space is new Security.Permissions.Definition ("wiki-space-update");

   --  ------------------------------
   --  Module wikis
   --  ------------------------------
   type Wiki_Module is new AWA.Modules.Module with private;
   type Wiki_Module_Access is access all Wiki_Module'Class;

   --  Initialize the wikis module.
   overriding
   procedure Initialize (Plugin : in out Wiki_Module;
                         App    : in AWA.Modules.Application_Access;
                         Props  : in ASF.Applications.Config);

   --  Get the wikis module.
   function Get_Wiki_Module return Wiki_Module_Access;

   --  Create the wiki space.
   procedure Create_Wiki_Space (Module : in Wiki_Module;
                                Wiki   : in out AWA.Wikis.Models.Wiki_Space_Ref'Class);

   --  Save the wiki space.
   procedure Save_Wiki_Space (Module : in Wiki_Module;
                              Wiki   : in out AWA.Wikis.Models.Wiki_Space_Ref'Class);

   --  Load the wiki space.
   procedure Load_Wiki_Space (Module : in Wiki_Module;
                              Wiki   : in out AWA.Wikis.Models.Wiki_Space_Ref'Class;
                              Id     : in ADO.Identifier);

   --  Create the wiki page into the wiki space.
   procedure Create_Wiki_Page (Model  : in Wiki_Module;
                               Into   : in AWA.Wikis.Models.Wiki_Space_Ref'Class;
                               Page   : in out Awa.Wikis.Models.Wiki_Page_Ref'Class);

   --  Save the wiki page.
   procedure Save (Model  : in Wiki_Module;
                   Page   : in out Awa.Wikis.Models.Wiki_Page_Ref'Class);

   --  Create a new wiki content for the wiki page.
   procedure Create_Wiki_Content (Model   : in Wiki_Module;
                                  Page    : in out Awa.Wikis.Models.Wiki_Page_Ref'Class;
                                  Content : in out AWA.Wikis.Models.Wiki_Content_Ref'Class);

private

   type Wiki_Module is new AWA.Modules.Module with null record;

end AWA.Wikis.Modules;
