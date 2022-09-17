-----------------------------------------------------------------------
--  awa-users-filters -- Specific filters for authentication and key verification
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
with Servlet.Filters;
with Servlet.Core;
with Servlet.Security.Filters;

with AWA.Applications;
with AWA.Users.Principals;
package AWA.Users.Filters is

   --  Set the user principal on the session associated with the ASF request.
   procedure Set_Session_Principal (Request   : in out Servlet.Requests.Request'Class;
                                    Principal : in AWA.Users.Principals.Principal_Access);

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

   --  ------------------------------
   --  Verify access key filter
   --  ------------------------------
   --  The <b>Verify_Filter</b> filter verifies an access key associated to a user.
   --  The access key should have been sent to the user by some mechanism (email).
   --  The access key must be valid, that is an <b>Access_Key</b> database entry
   --  must exist and it must be associated with an email address and a user.
   type Verify_Filter is new Servlet.Filters.Filter with private;

   --  The request parameter that <b>Verify_Filter</b> will check.
   PARAM_ACCESS_KEY : constant String := "key";

   --  The configuration parameter which controls the redirection page
   --  when the access key is invalid.
   VERIFY_FILTER_REDIRECT_PARAM : constant String := "redirect";

   --  Configuration parameter which controls the change password
   --  page when the access key is valid, the user is enabled but
   --  there is no password for authentication.
   VERIFY_FILTER_CHANGE_PASSWORD_PARAM : constant String := "change-password";

   --  Initialize the filter and configure the redirection URIs.
   overriding
   procedure Initialize (Filter  : in out Verify_Filter;
                         Config  : in Servlet.Core.Filter_Config);

   --  Filter a request which contains an access key and verify that the
   --  key is valid and identifies a user.  Once the user is known, create
   --  a session and setup the user principal.
   --
   --  If the access key is missing or invalid, redirect to the
   --  <b>Invalid_Key_URI</b> associated with the filter.
   overriding
   procedure Do_Filter (Filter   : in Verify_Filter;
                        Request  : in out Servlet.Requests.Request'Class;
                        Response : in out Servlet.Responses.Response'Class;
                        Chain    : in out Servlet.Core.Filter_Chain);

private

   use Ada.Strings.Unbounded;

   type Auth_Filter is new Servlet.Security.Filters.Auth_Filter with record
      Login_URI   : Unbounded_String;
      Application : AWA.Applications.Application_Access;
   end record;

   type Verify_Filter is new Servlet.Filters.Filter with record
      Invalid_Key_URI     : Unbounded_String;
      Change_Password_URI : Unbounded_String;
   end record;

end AWA.Users.Filters;
