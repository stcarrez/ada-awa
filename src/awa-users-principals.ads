-----------------------------------------------------------------------
--  awa-users-principals -- User principals
--  Copyright (C) 2011, 2012, 2013, 2014, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with ADO;
with AWA.Users.Models;
with ASF.Principals;
package AWA.Users.Principals is

   type Principal is new ASF.Principals.Principal with private;
   type Principal_Access is access all Principal'Class;

   --  Get the principal name.
   overriding
   function Get_Name (From : in Principal) return String;

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

   --  Create a principal for the given user.
   function Create (User    : in AWA.Users.Models.User_Ref;
                    Session : in AWA.Users.Models.Session_Ref) return Principal;

   --  Utility functions based on the security principal access type.

   --  Get the current user identifier invoking the service operation.
   --  Returns NO_IDENTIFIER if there is none or if the principal is not an AWA principal.
   function Get_User_Identifier (From : in ASF.Principals.Principal_Access)
                                 return ADO.Identifier;

private

   type Principal is new ASF.Principals.Principal with record
      User        : AWA.Users.Models.User_Ref;
      Session     : AWA.Users.Models.Session_Ref;
   end record;

end AWA.Users.Principals;
