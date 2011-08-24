-----------------------------------------------------------------------
--  awa-users-servlets -- OpenID verification servlet for user authentication
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

with ASF.Principals;
with Security.Openid.Servlets;
package AWA.Users.Servlets is

   --  ------------------------------
   --  OpenID Verification Servlet
   --  ------------------------------
   --  The <b>Verify_Auth_Servlet</b> verifies the authentication result and
   --  extract authentication from the callback URL.  We override the default implementation
   --  to provide our own user principal once the authentication succeeded.  At the same time,
   --  if this is the first time we see the user, s/he will be registered by using the
   --  user service.
   type Verify_Auth_Servlet is new Security.Openid.Servlets.Verify_Auth_Servlet with private;

   --  Create a principal object that correspond to the authenticated user identified
   --  by the <b>Auth</b> information.  The principal will be attached to the session
   --  and will be destroyed when the session is closed.
   procedure Create_Principal (Server : in Verify_Auth_Servlet;
                               Auth   : in Security.Openid.Authentication;
                               Result : out ASF.Principals.Principal_Access);

private

   type Verify_Auth_Servlet is new Security.Openid.Servlets.Verify_Auth_Servlet with null record;

end AWA.Users.Servlets;
