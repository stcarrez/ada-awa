-----------------------------------------------------------------------
--  awa-users-principals -- User principals
--  Copyright (C) 2011, 2012, 2013, 2014, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package body AWA.Users.Principals is

   --  ------------------------------
   --  Get the principal name.
   --  ------------------------------
   overriding
   function Get_Name (From : in Principal) return String is
   begin
      return From.User.Get_Name;
   end Get_Name;

   --  ------------------------------
   --  Get the principal identifier (name)
   --  ------------------------------
   function Get_Id (From : in Principal) return String is
   begin
      return From.User.Get_Name;
   end Get_Id;

   --  ------------------------------
   --  Get the user associated with the principal.
   --  ------------------------------
   function Get_User (From : in Principal) return AWA.Users.Models.User_Ref is
   begin
      return From.User;
   end Get_User;

   --  ------------------------------
   --  Get the current user identifier invoking the service operation.
   --  Returns NO_IDENTIFIER if there is none.
   --  ------------------------------
   function Get_User_Identifier (From : in Principal) return ADO.Identifier is
   begin
      return From.User.Get_Id;
   end Get_User_Identifier;

   --  ------------------------------
   --  Get the connection session used by the user.
   --  ------------------------------
   function Get_Session (From : in Principal) return AWA.Users.Models.Session_Ref is
   begin
      return From.Session;
   end Get_Session;

   --  ------------------------------
   --  Get the connection session identifier used by the user.
   --  ------------------------------
   function Get_Session_Identifier (From : in Principal) return ADO.Identifier is
   begin
      return From.Session.Get_Id;
   end Get_Session_Identifier;

   --  ------------------------------
   --  Create a principal for the given user.
   --  ------------------------------
   function Create (User    : in AWA.Users.Models.User_Ref;
                    Session : in AWA.Users.Models.Session_Ref) return Principal_Access is
      Result : constant Principal_Access := new Principal;
   begin
      Result.User    := User;
      Result.Session := Session;
      return Result;
   end Create;

   --  ------------------------------
   --  Create a principal for the given user.
   --  ------------------------------
   function Create (User    : in AWA.Users.Models.User_Ref;
                    Session : in AWA.Users.Models.Session_Ref) return Principal is
   begin
      return Result : Principal do
         Result.User    := User;
         Result.Session := Session;
      end return;
   end Create;

   --  ------------------------------
   --  Get the current user identifier invoking the service operation.
   --  Returns NO_IDENTIFIER if there is none or if the principal is not an AWA principal.
   --  ------------------------------
   function Get_User_Identifier (From : in ASF.Principals.Principal_Access)
                                 return ADO.Identifier is
      use type ASF.Principals.Principal_Access;
   begin
      if From = null then
         return ADO.NO_IDENTIFIER;

      elsif not (From.all in Principal'Class) then
         return ADO.NO_IDENTIFIER;

      else
         return Principal'Class (From.all).Get_User_Identifier;
      end if;
   end Get_User_Identifier;

end AWA.Users.Principals;
