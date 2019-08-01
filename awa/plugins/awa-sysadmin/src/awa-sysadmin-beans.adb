-----------------------------------------------------------------------
--  awa-sysadmin-beans -- Sysadmin specific Ada beans
--  Copyright (C) 2019 Stephane Carrez
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
with ASF.Applications.Messages.Factory;
with ASF.Contexts.Faces;
with Servlet.Sessions;
with Util.Beans.Objects;
with AWA.Sysadmin.Filters;
package body AWA.Sysadmin.Beans is

   --  ------------------------------
   --  Action to authenticate the sysadmin user.
   --  ------------------------------
   overriding
   procedure Authenticate (Data    : in out Authenticate_Bean;
                           Outcome : in out Unbounded_String) is
      Ctx      : constant ASF.Contexts.Faces.Faces_Context_Access := ASF.Contexts.Faces.Current;
      Password : constant String := To_String (Data.Password);
      Session  : Servlet.Sessions.Session := Ctx.Get_Session;
   begin
      if Password = "sysadmin" then
         Outcome := To_Unbounded_String ("success");
         Session.Set_Attribute (Filters.ADMIN_AUTH_BEAN,
                                Util.Beans.Objects.To_Object (True));
      else
         Outcome := To_Unbounded_String ("failure");
         Session.Remove_Attribute (Filters.ADMIN_AUTH_BEAN);
         ASF.Applications.Messages.Factory.Add_Message ("users.login_signup_fail_message");
      end if;
   end Authenticate;

   --  ------------------------------
   --  Create an authenticate bean.
   --  ------------------------------
   function Create_Authenticate_Bean (Module : in AWA.Sysadmin.Modules.Sysadmin_Module_Access)
                                      return Util.Beans.Basic.Readonly_Bean_Access is
      pragma Unreferenced (Module);

      Object : constant Authenticate_Bean_Access := new Authenticate_Bean;
   begin
      return Object.all'Access;
   end Create_Authenticate_Bean;

end AWA.Sysadmin.Beans;
