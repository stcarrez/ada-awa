-----------------------------------------------------------------------
--  awa-users-principals -- User principals
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

with ADO;
with AWA.Users.Models;
with ASF.Principals;
with Security.Permissions;
with Security.Openid.Servlets;
package AWA.Users.Principals is

   type Principal is new ASF.Principals.Principal with private;
   type Principal_Access is access all Principal'Class;

   --  Get the principal name.
   function Get_Name (From : in Principal) return String;

   --  Returns true if the given role is stored in the user principal.
   function Has_Role (User : in Principal;
                      Role : in Security.Permissions.Role_Type) return Boolean;

   --  Get the principal identifier (name)
   function Get_Id (From : in Principal) return String;

   --  Get the user associated with the principal.
   function Get_User (From : in Principal) return AWA.Users.Models.User_Ref;

   --  Get the current user identifier invoking the service operation.
   --  Returns NO_IDENTIFIER if there is none.
   function Get_User_Identifier (From : in Principal) return ADO.Identifier;

   --  Get the connection session used by the user.
   function Get_Session (From : in Principal) return AWA.Users.Models.Session_Ref;

   --  Get the connection session identifier used by the user.
   function Get_Session_Identifier (From : in Principal) return ADO.Identifier;

   --  Create a principal for the given user.
   function Create (User    : in AWA.Users.Models.User_Ref;
                    Session : in AWA.Users.Models.Session_Ref) return Principal_Access;


   --  Utility functions based on the security principal access type.

   --  Get the current user identifier invoking the service operation.
   --  Returns NO_IDENTIFIER if there is none or if the principal is not an AWA principal.
   function Get_User_Identifier (From : in Security.Permissions.Principal_Access)
                                 return ADO.Identifier;

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

   type Principal is new ASF.Principals.Principal with record
      User        : AWA.Users.Models.User_Ref;
      Session     : AWA.Users.Models.Session_Ref;
      Roles       : Security.Permissions.Role_Map;
   end record;

   type Verify_Auth_Servlet is new Security.Openid.Servlets.Verify_Auth_Servlet with null record;

end AWA.Users.Principals;
