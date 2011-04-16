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
with ASF.Principals;
with Security.Permissions;
package AWA.Users.Principals is

   type Principal is new ASF.Principals.Principal with private;
   type Principal_Access is access all Principal'Class;

   --  Get the principal name.
   function Get_Name (From : in Principal) return String;

   --  Returns true if the given permission is stored in the user principal.
   function Has_Permission (User       : in Principal;
                            Permission : in Security.Permissions.Permission_Type) return Boolean;

   function Get_Id (From : in Principal) return String;

   --  Create a principal for the given user.
   function Create (User : in AWA.Users.Model.User_Ref) return Principal_Access;

private

   type Principal is new ASF.Principals.Principal with record
      User        : AWA.Users.Model.User_Ref;
      Permissions : Security.Permissions.Permission_Map;
   end record;

end AWA.Users.Principals;
