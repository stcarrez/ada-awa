-----------------------------------------------------------------------
--  awa-comments-module -- Comments module
--  Copyright (C) 2011, 2012 Stephane Carrez
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
with AWA.Modules.Get;
with AWA.Comments.Services;
package AWA.Comments.Modules is

   NAME : constant String := "comments";

   type Comment_Module is new AWA.Modules.Module with null record;
   type Comment_Module_Access is access all Comment_Module'Class;

   overriding
   procedure Initialize (Plugin : in out Comment_Module;
                         App    : in AWA.Modules.Application_Access;
                         Props  : in ASF.Applications.Config);

   function Get_Comment_Module is
     new AWA.Modules.Get (Comment_Module, Comment_Module_Access, NAME);

   function Get_Comment_Manager is
     new AWA.Modules.Get_Manager (AWA.Comments.Services.Comment_Service,
                                  AWA.Comments.Services.Comment_Service_Access,
                                  "Comment_Manager");

end AWA.Comments.Modules;
