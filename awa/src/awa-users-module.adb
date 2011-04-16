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

with ASF.Modules.Beans;
with ASF.Beans;

with AWA.Users.Beans;
package body AWA.Users.Module is

   package Register is new ASF.Modules.Beans (Module => User_Module,
                                              Module_Access => User_Module_Access);

   Object : aliased User_Module;

   function Instance return User_Module_Access is
   begin
      return Object'Access;
   end Instance;

begin
   Register.Register (Plugin  => Object'Access,
                      Name    => "login",
                      Handler => AWA.Users.Beans.Create_Authenticate_Bean'Access,
                      Free    => AWA.Users.Beans.Free_Authenticate_Bean'Access,
                      Scope   => ASF.Beans.REQUEST_SCOPE);

   Register.Register (Plugin  => Object'Access,
                      Name    => "register",
                      Handler => AWA.Users.Beans.Create_Authenticate_Bean'Access,
                      Free    => AWA.Users.Beans.Free_Authenticate_Bean'Access,
                      Scope   => ASF.Beans.REQUEST_SCOPE);

   Register.Register (Plugin  => Object'Access,
                      Name    => "lostPassword",
                      Handler => AWA.Users.Beans.Create_Authenticate_Bean'Access,
                      Free    => AWA.Users.Beans.Free_Authenticate_Bean'Access,
                      Scope   => ASF.Beans.REQUEST_SCOPE);

end AWA.Users.Module;
