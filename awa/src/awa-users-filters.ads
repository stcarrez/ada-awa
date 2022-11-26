-----------------------------------------------------------------------
--  awa-users-filters -- Specific filters for authentication
--  Copyright (C) 2011, 2012, 2015, 2019, 2022 Stephane Carrez
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

with Ada.Strings.Unbounded;

with Servlet.Requests;
with Servlet.Responses;
with Servlet.Sessions;
with ASF.Principals;
with Servlet.Core;
with Servlet.Security.Filters;

with AWA.Applications;
package AWA.Users.Filters is

   --  ------------------------------
   --  Authentication verification filter
   --  ------------------------------
   --  The <b>Auth_Filter</b> verifies that the user has the permission to access
   --  a given page.  If the user is not logged, it tries to login automatically
   --  by using some persistent cookie.  When this fails, it redirects the
   --  user to a login page (configured by AUTH_FILTER_REDIRECT_PARAM property).
   type Auth_Filter is new Servlet.Security.Filters.Auth_Filter with private;

   --  The configuration parameter which controls the redirection page
   --  when the user is not logged (this should be the login page).
   AUTH_FILTER_REDIRECT_PARAM   : constant String := "redirect";

   --  A temporary cookie used to store the URL for redirection after the login is successful.
   REDIRECT_COOKIE              : constant String := "RURL";

   --  Initialize the filter and configure the redirection URIs.
   overriding
   procedure Initialize (Filter  : in out Auth_Filter;
                         Config  : in Servlet.Core.Filter_Config);

   --  Authenticate a user by using the auto-login cookie.  This procedure is called if the
   --  current session does not have any principal.  Based on the request and the optional
   --  auto-login cookie passed in <b>Auth_Id</b>, it should identify the user and return
   --  a principal object.  The principal object will be freed when the session is closed.
   --  If the user cannot be authenticated, the returned principal should be null.
   --
   --  The default implementation returns a null principal.
   overriding
   procedure Authenticate (F        : in Auth_Filter;
                           Request  : in out Servlet.Requests.Request'Class;
                           Response : in out Servlet.Responses.Response'Class;
                           Session  : in Servlet.Sessions.Session;
                           Auth_Id  : in String;
                           Principal : out ASF.Principals.Principal_Access);

   --  Display or redirects the user to the login page.  This procedure is called when
   --  the user is not authenticated.
   overriding
   procedure Do_Login (Filter   : in Auth_Filter;
                       Request  : in out Servlet.Requests.Request'Class;
                       Response : in out Servlet.Responses.Response'Class);

private

   use Ada.Strings.Unbounded;

   type Auth_Filter is new Servlet.Security.Filters.Auth_Filter with record
      Login_URI   : Unbounded_String;
      Application : AWA.Applications.Application_Access;
   end record;

end AWA.Users.Filters;
