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
with ASF.Applications;

with AWA.Modules;
with Atlas.Microblog.Models;
package Atlas.Microblog.Modules is

   --  The name under which the module is registered.
   NAME : constant String := "microblog";

   --  ------------------------------
   --  Module microblog
   --  ------------------------------
   type Microblog_Module is new AWA.Modules.Module with private;
   type Microblog_Module_Access is access all Microblog_Module'Class;

   --  Initialize the microblog module.
   overriding
   procedure Initialize (Plugin : in out Microblog_Module;
                         App    : in AWA.Modules.Application_Access;
                         Props  : in ASF.Applications.Config);

   --  Get the microblog module.
   function Get_Microblog_Module return Microblog_Module_Access;

   --  Create a post for the microblog.
   procedure Create (Plugin : in Microblog_Module;
                     Post   : in out Atlas.Microblog.Models.Mblog_Ref);

private

   type Microblog_Module is new AWA.Modules.Module with null record;

end Atlas.Microblog.Modules;
