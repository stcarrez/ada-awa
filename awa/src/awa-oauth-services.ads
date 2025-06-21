-----------------------------------------------------------------------
--  awa-oauth-services -- OAuth Server Side
--  Copyright (C) 2017 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Security.OAuth.Servers;
package AWA.OAuth.Services is

   type Auth_Manager is new Security.OAuth.Servers.Auth_Manager with private;
   type Auth_Manager_Access is access all Auth_Manager'Class;

private

   type Auth_Manager is new Security.OAuth.Servers.Auth_Manager with record
      N : Natural;
   end record;

end AWA.OAuth.Services;
