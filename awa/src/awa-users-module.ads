-----------------------------------------------------------------------
--  awa -- Ada Web Application
--  Copyright (C) 2009, 2010 Stephane Carrez
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

with AWA.Users.Logic;
package AWA.Users.Module is

   NAME : constant String := "users";

   type User_Module is new AWA.Modules.Module with private;
   type User_Module_Access is access all User_Module'Class;

   overriding
   procedure Initialize (Plugin : in out User_Module;
                         App    : access ASF.Applications.Main.Application'Class);

   --  Get the user manager.
   function Get_User_Manager (Plugin : in User_Module) return Logic.User_Manager_Access;

   --  Create a user manager.
   function Create_User_Manager (Plugin : in User_Module) return Logic.User_Manager_Access;

   --  Get the user module instance associated with the current application.
   function Get_User_Module return User_Module_Access;

   --  Get the user manager instance associated with the current application.
   function Get_User_Manager return Logic.User_Manager_Access;

private

   type User_Module is new AWA.Modules.Module with record
      Manager : Logic.User_Manager_Access := null;
   end record;

end AWA.Users.Module;
