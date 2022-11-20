-----------------------------------------------------------------------
--  awa-users-servlets -- OpenID verification servlet for user authentication
--  Copyright (C) 2011 - 2022 Stephane Carrez
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

with Servlet.Core;
with Servlet.Security.Servlets;
with Servlet.Requests;
with Servlet.Responses;
with Servlet.Sessions;
with Security.Auth;
with AWA.Users.Principals;
private with Ada.Strings.Unbounded;

--  == OAuth Authentication Flow ==
--  The OAuth/OpenID authentication flow is implemented by using two servlets
--  that participate in the authentication.  A first servlet will start
--  the OAuth/OpenID authentication by building the request that the user
--  must use to authenticate through the OAuth/OpenID authorization server.
--  This servlet is implemented by the `AWA.Users.Servlets.Request_Auth_Servlet`
--  type.  The servlet will respond to an HTTP `GET` request and it will
--  redirect the user to the authorization server.
--
--  ![OAuth Authentication Flow](images/OAuthAuthenticateFlow.png)
--
--  The user will be authenticated by the OAuth/OpenID authorization server
--  and when s/he grants the application to access his or her account,
--  a redirection is made to the second servlet.  The second servlet
--  is implemented by `AWA.Users.Servlets.Verify_Auth_Servlet`.  It is used
--  to validate the authentication result by checking its validity with
--  the OAuth/OpenID authorization endpoint.  During this step, we can
--  retrieve some minimal information that uniquely identifies the user
--  such as a unique identifier that is specific to the OAuth/OpenID
--  authorization server.  It is also possible to retrieve the
--  user's name and email address.
--
--  These two servlets are provided by the `User_Module` and they are
--  registered under the `openid-auth` name for the first step and
--  under the `openid-verify` name for the second step.
package AWA.Users.Servlets is

   AUTH_ERROR_ATTRIBUTE          : constant String := "authError";
   MESSAGE_BAD_CONFIGURATION     : constant String := "users.message_invalid_configuration";
   MESSAGE_AUTH_FAILED           : constant String := "users.message_invalid_auth";
   MESSAGE_REGISTRATION_DISABLED : constant String := "users.message_registration_disabled";

   --  Set the user principal on the session associated with the ASF request.
   procedure Set_Session_Principal (Request   : in out Servlet.Requests.Request'Class;
                                    Principal : in AWA.Users.Principals.Principal_Access);

   --  ------------------------------
   --  OpenID Request Servlet
   --  ------------------------------
   --  The `Request_Auth_Servlet` servlet implements the first steps of an OpenID
   --  authentication.
   type Request_Auth_Servlet is new Servlet.Security.Servlets.Request_Auth_Servlet
     with null record;

   --  Proceed to the OpenID authentication with an OpenID provider.
   --  Find the OpenID provider URL and starts the discovery, association phases
   --  during which a private key is obtained from the OpenID provider.
   --  After OpenID discovery and association, the user will be redirected to
   --  the OpenID provider.
   overriding
   procedure Do_Get (Server   : in Request_Auth_Servlet;
                     Request  : in out Servlet.Requests.Request'Class;
                     Response : in out Servlet.Responses.Response'Class);

   --  ------------------------------
   --  OpenID Verification Servlet
   --  ------------------------------
   --  The `Verify_Auth_Servlet` verifies the authentication result and
   --  extract authentication from the callback URL.  We override the default
   --  implementation to provide our own user principal once the authentication
   --  succeeded.  At the same time, if this is the first time we see the user,
   --  s/he will be registered by using the user service.
   type Verify_Auth_Servlet is new Servlet.Security.Servlets.Verify_Auth_Servlet with private;

   --  Create a principal object that correspond to the authenticated user
   --  identified by the `Auth` information.  The principal will be attached
   --  to the session and will be destroyed when the session is closed.
   overriding
   procedure Create_Principal (Server : in Verify_Auth_Servlet;
                               Auth   : in Security.Auth.Authentication;
                               Result : out Security.Principal_Access);

   --  Verify the authentication result that was returned by the OpenID provider.
   --  If the authentication succeeded and the signature was correct, sets a
   --  user principals on the session.
   overriding
   procedure Do_Get (Server   : in Verify_Auth_Servlet;
                     Request  : in out Servlet.Requests.Request'Class;
                     Response : in out Servlet.Responses.Response'Class);

   --  Get the redirection URL that must be used after the authentication succeeded.
   function Get_Redirect_URL (Server  : in Verify_Auth_Servlet;
                              Session : in Servlet.Sessions.Session'Class;
                              Request : in Servlet.Requests.Request'Class) return String;

   --  Get the redirection URL that must be used after the authentication failed.
   function Get_Error_URL (Server  : in Verify_Auth_Servlet;
                           Session : in Servlet.Sessions.Session'Class;
                           Request : in Servlet.Requests.Request'Class) return String;

   --  ------------------------------
   --  Verify access key
   --  ------------------------------
   --  The <b>Verify_Filter</b> filter verifies an access key associated to a user.
   --  The access key should have been sent to the user by some mechanism (email).
   --  The access key must be valid, that is an <b>Access_Key</b> database entry
   --  must exist and it must be associated with an email address and a user.
   type Verify_Key_Servlet is new Servlet.Core.Servlet with private;

   --  The request parameter that <b>Verify_Filter</b> will check.
   PARAM_ACCESS_KEY : constant String := "key";

   --  The configuration parameter which controls the redirection page
   --  when the access key is invalid.
   VERIFY_FILTER_REDIRECT_PARAM : constant String := "verify-access-key.redirect";

   --  Configuration parameter which controls the change password
   --  page when the access key is valid, the user is enabled but
   --  there is no password for authentication.
   VERIFY_FILTER_CHANGE_PASSWORD_PARAM : constant String := "verify-access-key.change-password";

   --  Get the redirection URL that must be used after the authentication succeeded.
   function Get_Redirect_URL (Server  : in Verify_Key_Servlet;
                              Request : in Servlet.Requests.Request'Class) return String;

   --  Called by the servlet container to indicate to a servlet that the servlet
   --  is being placed into service.
   overriding
   procedure Initialize (Server  : in out Verify_Key_Servlet;
                         Context : in Servlet.Core.Servlet_Registry'Class);

   --  Filter a request which contains an access key and verify that the
   --  key is valid and identifies a user.  Once the user is known, create
   --  a session and setup the user principal.
   --
   --  If the access key is missing or invalid, redirect to the
   --  <b>Invalid_Key_URI</b> associated with the filter.
   overriding
   procedure Do_Get (Server   : in Verify_Key_Servlet;
                     Request  : in out Servlet.Requests.Request'Class;
                     Response : in out Servlet.Responses.Response'Class);

private

   type Verify_Auth_Servlet is new Servlet.Security.Servlets.Verify_Auth_Servlet with null record;

   type Verify_Key_Servlet is new Servlet.Core.Servlet with record
      Invalid_Key_URI     : Ada.Strings.Unbounded.Unbounded_String;
      Change_Password_URI : Ada.Strings.Unbounded.Unbounded_String;
   end record;

end AWA.Users.Servlets;
