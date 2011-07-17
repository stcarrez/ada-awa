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

with Util.Log.Loggers;
package body AWA.Comments.Module is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("AWA.Comments.Module");

   overriding
   procedure Initialize (Plugin : in out Comment_Module;
                         App    : access ASF.Applications.Main.Application'Class) is
   begin
      Log.Info ("Initializing the comments module");

      --  Setup the resource bundles.
      App.Register ("userMsg", "users");

--        Plugin.Manager := Plugin.Create_User_Manager;
--        Register.Register (Plugin  => Plugin,
--                           Name    => "AWA.Users.Beans.Authenticate_Bean",
--                           Handler => AWA.Users.Beans.Create_Authenticate_Bean'Access);

      AWA.Modules.Module (Plugin).Initialize (App);
   end Initialize;

end AWA.Comments.Module;
