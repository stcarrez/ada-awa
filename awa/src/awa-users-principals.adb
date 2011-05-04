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

with AWA.Users.Model;
with AWA.Users.Logic;
with AWA.Users.Module;
with ASF.Principals;
package body AWA.Users.Principals is

   --  ------------------------------
   --  Get the principal name.
   --  ------------------------------
   function Get_Name (From : in Principal) return String is
   begin
      return From.User.Get_Name;
   end Get_Name;

   --  Returns true if the given permission is stored in the user principal.
   function Has_Permission (User       : in Principal;
                            Permission : in Security.Permissions.Permission_Type) return Boolean is
   begin
      return User.Permissions (Permission);
   end Has_Permission;

   function Get_Id (From : in Principal) return String is
   begin
      return From.User.Get_Name;
   end Get_Id;

   --  ------------------------------
   --  Create a principal for the given user.
   --  ------------------------------
   function Create (User : in AWA.Users.Model.User_Ref) return Principal_Access is
      Result : constant Principal_Access := new Principal;
   begin
      Result.User := User;
      return Result;
   end Create;

   --  ------------------------------
   --  Create a principal object that correspond to the authenticated user identified
   --  by the <b>Auth</b> information.  The principal will be attached to the session
   --  and will be destroyed when the session is closed.
   --  ------------------------------
   procedure Create_Principal (Server : in Verify_Auth_Servlet;
                               Auth   : in Security.Openid.Authentication;
                               Result : out ASF.Principals.Principal_Access) is
      use AWA.Users.Module;
      use AWA.Users.Logic;
      Module  : constant User_Module_Access := AWA.Users.Module.Instance;
      Manager : User_Manager_Access := AWA.Users.Logic.Create_User_Manager (Module);
      User    : AWA.Users.Principals.Principal_Access := new AWA.Users.Principals.Principal;
   begin
      Result := User.all'Access;
      Manager.Authenticate (OpenId  => Security.Openid.Get_Claimed_Id (Auth),
                            Email   => Security.Openid.Get_Email (Auth),
                            Name    => Security.Openid.Get_Full_Name (Auth),
                            IpAddr  => "",
                            User    => User.User,
                            Session => User.Session);
   end Create_Principal;

end AWA.Users.Principals;
