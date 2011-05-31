-----------------------------------------------------------------------
--  awa-comments-module -- Comments module
--  Copyright (C) 2011 Stephane Carrez
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

with AWA.Modules;
with ASF.Modules.Get;
with ASF.Applications.Main;
with AWA.Comments.Services;
package AWA.Comments.Module is

   NAME : constant String := "Comment_Module";

   type Comment_Module is new AWA.Modules.Module with null record;
   type Comment_Module_Access is access all Comment_Module'Class;

   overriding
   procedure Initialize (Plugin : in out Comment_Module;
                         App    : access ASF.Applications.Main.Application'Class);

   function Get_Comment_Module is
     new ASF.Modules.Get (Comment_Module, Comment_Module_Access, NAME);

   function Get_Comment_Manager is
     new AWA.Modules.Get_Manager (AWA.Comments.Services.Comment_Manager,
                                  AWA.Comments.Services.Comment_Manager_Access,
                                  "Comment_Manager");

end AWA.Comments.Module;
