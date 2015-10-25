-----------------------------------------------------------------------
--  awa-users-servlets -- OpenID verification servlet for user authentication
--  Copyright (C) 2011, 2012, 2013, 2014, 2015 Stephane Carrez
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

with ASF.Principals;
with ASF.Security.Servlets;
with ASF.Requests;
with ASF.Responses;
with ASF.Sessions;
with Security.Auth;
package AWA.Users.Servlets is

   --  ------------------------------
   --  OpenID Request Servlet
   --  ------------------------------
   --  The <b>Request_Auth_Servlet</b> servlet implements the first steps of an OpenID
   --  authentication.
   type Request_Auth_Servlet is new ASF.Security.Servlets.Request_Auth_Servlet with null record;

   --  Proceed to the OpenID authentication with an OpenID provider.
   --  Find the OpenID provider URL and starts the discovery, association phases
   --  during which a private key is obtained from the OpenID provider.
   --  After OpenID discovery and association, the user will be redirected to
   --  the OpenID provider.
   overriding
   procedure Do_Get (Server   : in Request_Auth_Servlet;
                     Request  : in out ASF.Requests.Request'Class;
                     Response : in out ASF.Responses.Response'Class);

   --  ------------------------------
   --  OpenID Verification Servlet
   --  ------------------------------
   --  The <b>Verify_Auth_Servlet</b> verifies the authentication result and
   --  extract authentication from the callback URL.  We override the default implementation
   --  to provide our own user principal once the authentication succeeded.  At the same time,
   --  if this is the first time we see the user, s/he will be registered by using the
   --  user service.
   type Verify_Auth_Servlet is new ASF.Security.Servlets.Verify_Auth_Servlet with private;

   --  Create a principal object that correspond to the authenticated user identified
   --  by the <b>Auth</b> information.  The principal will be attached to the session
   --  and will be destroyed when the session is closed.
   overriding
   procedure Create_Principal (Server : in Verify_Auth_Servlet;
                               Auth   : in Security.Auth.Authentication;
                               Result : out ASF.Principals.Principal_Access);

   --  Verify the authentication result that was returned by the OpenID provider.
   --  If the authentication succeeded and the signature was correct, sets a
   --  user principals on the session.
   overriding
   procedure Do_Get (Server   : in Verify_Auth_Servlet;
                     Request  : in out ASF.Requests.Request'Class;
                     Response : in out ASF.Responses.Response'Class);

   --  Get the redirection URL that must be used after the authentication succeeded.
   function Get_Redirect_URL (Server  : in Verify_Auth_Servlet;
                              Session : in ASF.Sessions.Session'Class;
                              Request : in ASF.Requests.Request'Class) return String;

private

   type Verify_Auth_Servlet is new ASF.Security.Servlets.Verify_Auth_Servlet with null record;

end AWA.Users.Servlets;
