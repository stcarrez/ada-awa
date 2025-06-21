-----------------------------------------------------------------------
--  security-auth-fake -- A fake OAuth provider for unit tests
--  Copyright (C) 2020, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package body Security.Auth.Fake is

   --  Initialize the OpenID authentication realm.  Get the <tt>openid.realm</tt>
   --  and <tt>openid.callback_url</tt> parameters to configure the realm.
   overriding
   procedure Initialize (Realm  : in out Manager;
                         Params : in Parameters'Class;
                         Name   : in String := PROVIDER_OPENID) is
      pragma Unreferenced (Realm, Params, Name);
   begin
      null;
   end Initialize;

   --  Discover the OpenID provider that must be used to authenticate the user.
   --  The <b>Name</b> can be an URL or an alias that identifies the provider.
   --  A cached OpenID provider can be returned.
   --  Read the XRDS document from the URI and initialize the OpenID provider end point.
   --  (See OpenID Section 7.3 Discovery)
   overriding
   procedure Discover (Realm  : in out Manager;
                       Name   : in String;
                       Result : out End_Point) is
      pragma Unreferenced (Realm, Name, Result);
   begin
      null;
   end Discover;

   --  Associate the application (relying party) with the OpenID provider.
   --  The association can be cached.
   --  (See OpenID Section 8 Establishing Associations)
   overriding
   procedure Associate (Realm  : in out Manager;
                        OP     : in End_Point;
                        Result : out Association) is
      pragma Unreferenced (Realm, OP, Result);
   begin
      null;
   end Associate;

   --  Get the authentication URL to which the user must be redirected for authentication
   --  by the authentication server.
   overriding
   function Get_Authentication_URL (Realm : in Manager;
                                    OP    : in End_Point;
                                    Assoc : in Association) return String is
      pragma Unreferenced (Realm, OP, Assoc);
   begin
      return "/fake-authorization-server";
   end Get_Authentication_URL;

   --  Verify the authentication result
   overriding
   procedure Verify (Realm   : in out Manager;
                     Assoc   : in Association;
                     Request : in Parameters'Class;
                     Result  : out Authentication) is
      pragma Unreferenced (Realm, Assoc);
      Email      : constant String := Request.Get_Parameter ("email");
      Claimed_Id : constant String := Request.Get_Parameter ("claimed_id");
      Identity   : constant String := Request.Get_Parameter ("id");
   begin
      if Email'Length = 0 or else Identity'Length = 0 then
         Result.Status := Security.Auth.CANCEL;
      else
         Result.Status := Security.Auth.AUTHENTICATED;
      end if;
      Result.Identity := To_Unbounded_String (Identity);
      Result.Claimed_Id := To_Unbounded_String (Claimed_Id);
      Result.Email := To_Unbounded_String (Email);
   end Verify;

end Security.Auth.Fake;
