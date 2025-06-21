-----------------------------------------------------------------------
--  awa-sysadmin-beans -- Sysadmin specific Ada beans
--  Copyright (C) 2019 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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
