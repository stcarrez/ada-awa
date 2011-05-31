-----------------------------------------------------------------------
--  awa-users-beans -- ASF Beans for user module
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

with Ada.Strings.Unbounded;

with Util.Log.Loggers;

with AWA.Users.Services;
with AWA.Users.Module;
with ASF.Filters;
with ASF.Requests;
with ASF.Responses;
with ASF.Servlets;
with ASF.Sessions;
with ASF.Cookies;
with ASF.Principals;
with AWA.Users.Principals;
package body AWA.Users.Filters is

   use Util.Log;

   --  The logger
   Log : constant Loggers.Logger := Loggers.Create ("AWA.Users.Filters");


   procedure Authenticate (F        : in Auth_Filter;
                           Request  : in out ASF.Requests.Request'Class;
                           Response : in out ASF.Responses.Response'Class;
                           Session  : in ASF.Sessions.Session;
                           Auth_Id  : in String;
                           Principal : out ASF.Principals.Principal_Access) is
   begin
      null;
   end Authenticate;

end AWA.Users.Filters;
