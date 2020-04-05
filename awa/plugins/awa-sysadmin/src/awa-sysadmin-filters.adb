-----------------------------------------------------------------------
--  awa-users-filters -- Specific filters for authentication and key verification
--  Copyright (C) 2011, 2012, 2015, 2019 Stephane Carrez
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
with Util.Beans.Objects;

with Servlet.Sessions;
with Servlet.Cookies;

with AWA.Users.Filters;
package body AWA.Sysadmin.Filters is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("AWA.Sysadmin.Filters");

   --  ------------------------------
   --  Initialize the filter and configure the redirection URIs.
   --  ------------------------------
   overriding
   procedure Initialize (Filter  : in out Auth_Filter;
                         Config  : in Servlet.Core.Filter_Config) is
      URI : constant String
        := Servlet.Core.Get_Init_Parameter (Config,
                                            AWA.Users.Filters.AUTH_FILTER_REDIRECT_PARAM);
   begin
      Filter.Login_URI := To_Unbounded_String (URI);
   end Initialize;

   --  ------------------------------
   --  Display or redirects the user to the login page.  This procedure is called when
   --  the user is not authenticated.
   --  ------------------------------
   procedure Do_Login (Filter   : in Auth_Filter;
                       Request  : in out Servlet.Requests.Request'Class;
                       Response : in out Servlet.Responses.Response'Class) is
      use Servlet;

      Login_URI : constant String := To_String (Filter.Login_URI);
      Context   : constant String := Request.Get_Context_Path;
      Servlet   : constant String := Request.Get_Servlet_Path;
      URL       : constant String := Context & Servlet & Request.Get_Path_Info;
      C         : Cookies.Cookie
        := Cookies.Create (AWA.Users.Filters.REDIRECT_COOKIE, URL);
   begin
      Log.Info ("Sysadmin user is not logged, redirecting to {0}", Login_URI);

      Cookies.Set_Path (C, Request.Get_Context_Path);
      Cookies.Set_Max_Age (C, 86400);
      Response.Add_Cookie (Cookie => C);
      if Request.Get_Header ("X-Requested-With") = "" then
         Response.Send_Redirect (Location => Context & Login_URI);
      else
         Response.Send_Error (Responses.SC_UNAUTHORIZED);
      end if;
   end Do_Login;

   --  ------------------------------
   --  Filter a request which contains an access key and verify that the
   --  key is valid and identifies a user.  Once the user is known, create
   --  a session and setup the user principal.
   --
   --  If the access key is missing or invalid, redirect to the
   --  <b>Invalid_Key_URI</b> associated with the filter.
   --  ------------------------------
   overriding
   procedure Do_Filter (Filter   : in Auth_Filter;
                        Request  : in out Servlet.Requests.Request'Class;
                        Response : in out Servlet.Responses.Response'Class;
                        Chain    : in out Servlet.Core.Filter_Chain) is
      Session  : constant Servlet.Sessions.Session := Request.Get_Session;
      Is_Admin : Util.Beans.Objects.Object;
   begin
      if not Session.Is_Valid then
         Response.Set_Status (Servlet.Responses.SC_UNAUTHORIZED);
         return;
      end if;

      Is_Admin := Session.Get_Attribute (ADMIN_AUTH_BEAN);
      if Util.Beans.Objects.Is_Null (Is_Admin) then
         if Filter.Login_URI /= Request.Get_Path then
            Filter.Do_Login (Request, Response);
            return;
         end if;
      end if;

      --  Request is authorized, proceed to the next filter.
      Servlet.Core.Do_Filter (Chain    => Chain,
                              Request  => Request,
                              Response => Response);
   end Do_Filter;

end AWA.Sysadmin.Filters;
