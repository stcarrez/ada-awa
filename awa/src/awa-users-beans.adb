-----------------------------------------------------------------------
--  awa-users-beans -- ASF Beans for user module
--  Copyright (C) 2011, 2012, 2013, 2018, 2020, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Log.Loggers;

with Security;
with Servlet.Sessions;
with ASF.Contexts.Faces;
with ASF.Contexts.Flash;
with ASF.Cookies;
with ASF.Applications.Messages.Factory;
with ASF.Security.Filters;

with ADO.Sessions;

with AWA.Services.Contexts;
with AWA.Users.Servlets;
with AWA.Users.Filters;
package body AWA.Users.Beans is

   use AWA.Users.Models;
   use ASF.Applications;
   package UBO renames Util.Beans.Objects;

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("AWA.Users.Beans");

   --  Helper to send a remove cookie in the current response
   procedure Remove_Cookie (Name : in String);

   --  ------------------------------
   --  Action to register a user
   --  ------------------------------
   overriding
   procedure Register (Data    : in out Authenticate_Bean;
                       Outcome : in out Unbounded_String) is
      User  : User_Ref;
      Email : Email_Ref;
      Ctx   : constant ASF.Contexts.Faces.Faces_Context_Access := ASF.Contexts.Faces.Current;
      Flash : constant ASF.Contexts.Faces.Flash_Context_Access := Ctx.Get_Flash;
      Key   : constant String := To_String (Data.Access_Key);
      Principal : AWA.Users.Principals.Principal_Access;
   begin
      Email.Set_Email (Data.Email);
      User.Set_First_Name (Data.First_Name);
      User.Set_Last_Name (Data.Last_Name);
      if Key'Length > 0 then
         Data.Manager.Create_User (User      => User,
                                   Email     => Email,
                                   Key       => Key,
                                   Password  => To_String (Data.Password),
                                   IpAddr    => "",
                                   Principal => Principal);
         Outcome := To_Unbounded_String ("success-key");
         Data.Set_Session_Principal (Principal);
         Data.Set_Authenticate_Cookie (Principal);

         --  Add a message to the flash context so that it will be displayed on the next page.
         Flash.Set_Keep_Messages (True);
         Messages.Factory.Add_Message (Ctx.all, "users.message_registration_done", Messages.INFO);
      else
         declare
            Access_Key : Access_Key_Ref;
         begin
            Data.Manager.Create_User (User  => User,
                                      Email => Email,
                                      Key   => Access_Key,
                                      Password => To_String (Data.Password),
                                      Send  => True);
         end;
         Outcome := To_Unbounded_String ("success");

         --  Add a message to the flash context so that it will be displayed on the next page.
         Flash.Set_Keep_Messages (True);
         Messages.Factory.Add_Message (Ctx.all, "users.message_signup_sent", Messages.INFO);
      end if;

   exception
      when Services.User_Exist =>
         Outcome := To_Unbounded_String ("failure");

         Messages.Factory.Add_Message (Ctx.all, "users.signup_error_message");

      when Services.Registration_Disabled =>
         Outcome := To_Unbounded_String ("disabled");

         --  Add a message to the flash context so that it will be displayed on the error page.
         Flash.Set_Keep_Messages (True);
         Messages.Factory.Add_Message (Ctx.all, "users.message_registration_disabled",
                                       Messages.INFO);

   end Register;

   --  ------------------------------
   --  Action to trigger the lost password email process.
   --  ------------------------------
   overriding
   procedure Lost_Password (Data    : in out Authenticate_Bean;
                            Outcome : in out Unbounded_String) is
      Ctx   : constant ASF.Contexts.Faces.Faces_Context_Access := ASF.Contexts.Faces.Current;
      Flash : constant ASF.Contexts.Faces.Flash_Context_Access := Ctx.Get_Flash;
   begin
      Data.Manager.Lost_Password (Email => To_String (Data.Email));
      Outcome := To_Unbounded_String ("success");

      --  Add a message to the flash context so that it will be displayed on the next page.
      Flash.Set_Keep_Messages (True);
      Messages.Factory.Add_Message (Ctx.all, "users.message_lost_password_sent", Messages.INFO);

   exception
      when Services.Not_Found =>
         Messages.Factory.Add_Message (Ctx.all, "users.error_email_not_found");

   end Lost_Password;

   --  ------------------------------
   --  Action to validate the reset password key and set a new password.
   --  ------------------------------
   overriding
   procedure Reset_Password (Data    : in out Authenticate_Bean;
                             Outcome : in out Unbounded_String) is
      Ctx   : constant ASF.Contexts.Faces.Faces_Context_Access := ASF.Contexts.Faces.Current;
      Flash : constant ASF.Contexts.Faces.Flash_Context_Access := Ctx.Get_Flash;
      Principal : AWA.Users.Principals.Principal_Access;
   begin
      Data.Manager.Reset_Password (Key       => To_String (Data.Access_Key),
                                   Password  => To_String (Data.Password),
                                   IpAddr    => "",
                                   Principal => Principal);
      Data.Set_Session_Principal (Principal);
      Outcome := To_Unbounded_String ("success");

      --  Add a message to the flash context so that it will be displayed on the next page.
      Flash.Set_Keep_Messages (True);
      Messages.Factory.Add_Message (Ctx.all, "users.message_reset_password_done", Messages.INFO);

   exception
      when Services.Not_Found =>
         Messages.Factory.Add_Message (Ctx.all, "users.error_reset_password");
   end Reset_Password;

   procedure Set_Session_Principal (Data      : in Authenticate_Bean;
                                    Principal : in AWA.Users.Principals.Principal_Access) is
      pragma Unreferenced (Data);

      Ctx       : constant ASF.Contexts.Faces.Faces_Context_Access := ASF.Contexts.Faces.Current;
      Session   : Servlet.Sessions.Session := Ctx.Get_Session (Create => True);
   begin
      Session.Set_Principal (Principal.all'Access);
   end Set_Session_Principal;

   procedure Set_Authenticate_Cookie (Data    : in out Authenticate_Bean;
                                      Principal : in AWA.Users.Principals.Principal_Access) is
      Id     : constant ADO.Identifier := Principal.Get_Session_Identifier;
      Cookie : constant String := Data.Manager.Get_Authenticate_Cookie (Id);
      Ctx    : constant ASF.Contexts.Faces.Faces_Context_Access := ASF.Contexts.Faces.Current;
      C      : ASF.Cookies.Cookie := ASF.Cookies.Create (ASF.Security.Filters.AID_COOKIE, Cookie);
   begin
      ASF.Cookies.Set_Path (C, Ctx.Get_Request.Get_Context_Path);
      ASF.Cookies.Set_Max_Age (C, 15 * 86400);
      Ctx.Get_Response.Add_Cookie (Cookie => C);
   end Set_Authenticate_Cookie;

   --  ------------------------------
   --  Action to authenticate a user (password authentication).
   --  ------------------------------
   overriding
   procedure Authenticate (Data    : in out Authenticate_Bean;
                           Outcome : in out Unbounded_String) is
      Principal : AWA.Users.Principals.Principal_Access;
   begin
      Data.Manager.Authenticate (Email    => To_String (Data.Email),
                                 Password => To_String (Data.Password),
                                 IpAddr   => "",
                                 Principal => Principal);
      Outcome := To_Unbounded_String ("success");

      Data.Set_Session_Principal (Principal);
      Data.Set_Authenticate_Cookie (Principal);
      Remove_Cookie (AWA.Users.Filters.REDIRECT_COOKIE);
      if Length (Data.Redirect) > 0 then
         declare
            Context : constant ASF.Contexts.Faces.Faces_Context_Access
              := ASF.Contexts.Faces.Current;
         begin
            Context.Get_Response.Send_Redirect
              (Location => To_String (Data.Redirect));
         end;
      end if;

   exception
      when Services.Not_Found =>
         Outcome := To_Unbounded_String ("failure");

         ASF.Applications.Messages.Factory.Add_Message
           ("users.login_signup_fail_message");

      when Services.User_Disabled =>
         Outcome := To_Unbounded_String ("failure");

         ASF.Applications.Messages.Factory.Add_Message
           ("users.login_signup_account_disabled_message");
   end Authenticate;

   --  ------------------------------
   --  Helper to send a remove cookie in the current response
   --  ------------------------------
   procedure Remove_Cookie (Name : in String) is
      Ctx : constant ASF.Contexts.Faces.Faces_Context_Access := ASF.Contexts.Faces.Current;
      C   : ASF.Cookies.Cookie := ASF.Cookies.Create (Name, "");
   begin
      ASF.Cookies.Set_Path (C, Ctx.Get_Request.Get_Context_Path);
      ASF.Cookies.Set_Max_Age (C, 0);
      Ctx.Get_Response.Add_Cookie (Cookie => C);
   end Remove_Cookie;

   --  ------------------------------
   --  Logout the user and closes the session.
   --  ------------------------------
   overriding
   procedure Logout (Data    : in out Authenticate_Bean;
                     Outcome : in out Unbounded_String) is
      use type Security.Principal_Access;

      Ctx       : constant ASF.Contexts.Faces.Faces_Context_Access := ASF.Contexts.Faces.Current;
      Session   : Servlet.Sessions.Session := Ctx.Get_Session (Create => False);
   begin
      Outcome := To_Unbounded_String ("success");

      --  If there is no session, we are done.
      if not Session.Is_Valid then
         return;
      end if;

      declare
         Principal : constant Security.Principal_Access := Session.Get_Principal;
      begin
         if Principal /= null and then Principal.all in AWA.Users.Principals.Principal'Class then
            declare
               P : constant AWA.Users.Principals.Principal_Access :=
                 AWA.Users.Principals.Principal'Class (Principal.all)'Access;
            begin
               Data.Manager.Close_Session (Id => P.Get_Session_Identifier,
                                           Logout => True);

            exception
               when others =>
                  Log.Error ("Exception when closing user session...");
            end;
         end if;
         Ctx.Get_Application.Delete_Session (Session);
      end;

      --  Remove the session cookie.
      Remove_Cookie (ASF.Security.Filters.SID_COOKIE);
      Remove_Cookie (ASF.Security.Filters.AID_COOKIE);
   end Logout;

   --  ------------------------------
   --  Helper to send a remove cookie in the current response
   --  ------------------------------
   overriding
   procedure Load (Data    : in out Authenticate_Bean;
                   Outcome : in out Unbounded_String) is
      User  : User_Ref;
      Email : Email_Ref;
   begin
      Data.Manager.Load_User (User, Email, To_String (Data.Access_Key));
      Data.First_Name := User.Get_First_Name;
      Data.Last_Name := User.Get_Last_Name;
      Data.Email := Email.Get_Email;

   exception
      when others =>
         Outcome := Ada.Strings.Unbounded.To_Unbounded_String ("not-found");
   end Load;

   overriding
   procedure Auth_Error (Data    : in out Authenticate_Bean;
                         Outcome : in out Unbounded_String) is
      pragma Unreferenced (Data);

      Ctx       : constant ASF.Contexts.Faces.Faces_Context_Access := ASF.Contexts.Faces.Current;
      Session   : Servlet.Sessions.Session := Ctx.Get_Session (Create => False);
   begin
      Outcome := To_Unbounded_String ("login");
      if not Session.Is_Valid then
         return;
      end if;
      declare
         Err : constant UBO.Object
           := Session.Get_Attribute (AWA.Users.Servlets.AUTH_ERROR_ATTRIBUTE);
      begin
         Session.Remove_Attribute (AWA.Users.Servlets.AUTH_ERROR_ATTRIBUTE);
         if UBO.Is_Null (Err) then
            return;
         end if;
         ASF.Applications.Messages.Factory.Add_Message (UBO.To_String (Err));
      end;
   end Auth_Error;

   --  ------------------------------
   --  Create an authenticate bean.
   --  ------------------------------
   function Create_Authenticate_Bean (Module : in AWA.Users.Modules.User_Module_Access)
                                      return Util.Beans.Basic.Readonly_Bean_Access is
      Object : constant Authenticate_Bean_Access := new Authenticate_Bean;
   begin
      Object.Module := Module;
      Object.Manager := AWA.Users.Modules.Get_User_Manager;
      return Object.all'Access;
   end Create_Authenticate_Bean;

   --  ------------------------------
   --  Get the value identified by the name.
   --  ------------------------------
   overriding
   function Get_Value (From : in Authenticate_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = EMAIL_ATTR then
         return Util.Beans.Objects.To_Object (From.Email);
      elsif Name = PASSWORD_ATTR then
         return Util.Beans.Objects.To_Object (From.Password);
      elsif Name = FIRST_NAME_ATTR then
         return Util.Beans.Objects.To_Object (From.First_Name);
      elsif Name = LAST_NAME_ATTR then
         return Util.Beans.Objects.To_Object (From.Last_Name);
      elsif Name = KEY_ATTR then
         return Util.Beans.Objects.To_Object (From.Access_Key);
      elsif Name = REDIRECT_ATTR then
         return Util.Beans.Objects.To_Object (From.Redirect);
      else
         return Util.Beans.Objects.Null_Object;
      end if;
   end Get_Value;

   --  ------------------------------
   --  Set the value identified by the name.
   --  ------------------------------
   overriding
   procedure Set_Value (From  : in out Authenticate_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if Name = EMAIL_ATTR then
         From.Email := Util.Beans.Objects.To_Unbounded_String (Value);
      elsif Name = PASSWORD_ATTR then
         From.Password := Util.Beans.Objects.To_Unbounded_String (Value);
      elsif Name = FIRST_NAME_ATTR then
         From.First_Name := Util.Beans.Objects.To_Unbounded_String (Value);
      elsif Name = LAST_NAME_ATTR then
         From.Last_Name := Util.Beans.Objects.To_Unbounded_String (Value);
      elsif Name = KEY_ATTR then
         From.Access_Key := Util.Beans.Objects.To_Unbounded_String (Value);
      elsif Name = REDIRECT_ATTR then
         From.Redirect := Util.Beans.Objects.To_Unbounded_String (Value);
      end if;
   end Set_Value;

   --  ------------------------------
   --  Get the value identified by the name.  The following names are recognized:
   --    o isLogged  Boolean  True if a user is logged
   --    o name      String   The user name
   --    o email     String   The user email address
   --    o id        Long     The user identifier
   --  ------------------------------
   overriding
   function Get_Value (From : in Current_User_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
      package ASC renames AWA.Services.Contexts;

      pragma Unreferenced (From);
      use type ASC.Service_Context_Access;

      Ctx : constant ASC.Service_Context_Access := ASC.Current;
   begin
      if Ctx = null then
         if Name = IS_LOGGED_ATTR then
            return Util.Beans.Objects.To_Object (False);
         else
            return Util.Beans.Objects.Null_Object;
         end if;
      else
         declare
            User : constant AWA.Users.Models.User_Ref := Ctx.Get_User;
         begin
            if User.Is_Null then
               if Name = IS_LOGGED_ATTR then
                  return Util.Beans.Objects.To_Object (False);
               else
                  return Util.Beans.Objects.Null_Object;
               end if;
            else
               if Name = USER_NAME_ATTR then
                  return Util.Beans.Objects.To_Object (String '(User.Get_Name));

               elsif Name = USER_EMAIL_ATTR then
                  declare
                     Email : AWA.Users.Models.Email_Ref'Class := User.Get_Email;
                  begin
                     if not Email.Is_Loaded then
                        --  The email object was not loaded.  Load it using a new database session.
                        declare
                           Session : ADO.Sessions.Session := ASC.Get_Session (Ctx);
                        begin
                           Email.Load (Session, Email.Get_Id);
                        end;
                     end if;
                     return Util.Beans.Objects.To_Object (String '(Email.Get_Email));
                  end;

               elsif Name = USER_ID_ATTR then
                  return Util.Beans.Objects.To_Object (Long_Long_Integer (User.Get_Id));

               elsif Name = IS_LOGGED_ATTR then
                  return Util.Beans.Objects.To_Object (True);

               else
                  return Util.Beans.Objects.Null_Object;
               end if;
            end if;
         end;
      end if;
   end Get_Value;

   --  ------------------------------
   --  Create the current user bean.
   --  ------------------------------
   function Create_Current_User_Bean (Module : in AWA.Users.Modules.User_Module_Access)
                                      return Util.Beans.Basic.Readonly_Bean_Access is
      pragma Unreferenced (Module);

      Object : constant Current_User_Bean_Access := new Current_User_Bean;
   begin
      return Object.all'Access;
   end Create_Current_User_Bean;

end AWA.Users.Beans;
