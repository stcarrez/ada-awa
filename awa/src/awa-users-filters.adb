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
with AWA.Users.Models;
package body AWA.Users.Filters is

   use Util.Log;

   --  The logger
   Log : constant Loggers.Logger := Loggers.Create ("AWA.Users.Filters");


   procedure Set_Session_Principal (Request : in out ASF.Requests.Request'Class;
                                    User    : in AWA.Users.Models.User_Ref;
                                    Sess    : in AWA.Users.Models.Session_Ref) is
      Principal : constant Principals.Principal_Access := Principals.Create (User, Sess);
      Session   : ASF.Sessions.Session := Request.Get_Session (Create => True);
   begin
      Session.Set_Principal (Principal.all'Access);
   end Set_Session_Principal;

   procedure Authenticate (F        : in Auth_Filter;
                           Request  : in out ASF.Requests.Request'Class;
                           Response : in out ASF.Responses.Response'Class;
                           Session  : in ASF.Sessions.Session;
                           Auth_Id  : in String;
                           Principal : out ASF.Principals.Principal_Access) is
   begin
      null;
   end Authenticate;

   --  ------------------------------
   --  Initialize the filter and configure the redirection URIs.
   --  ------------------------------
   procedure Initialize (Filter  : in out Verify_Filter;
                         Context : in ASF.Servlets.Servlet_Registry'Class) is
      URI : constant String := Context.Get_Init_Parameter ("user.verif-filter.redirect");
   begin
      Filter.Invalid_Key_URI := To_Unbounded_String (URI);
   end Initialize;

   --  ------------------------------
   --  Filter a request which contains an access key and verify that the
   --  key is valid and identifies a user.  Once the user is known, create
   --  a session and setup the user principal.
   --
   --  If the access key is missing or invalid, redirect to the
   --  <b>Invalid_Key_URI</b> associated with the filter.
   --  ------------------------------
   procedure Do_Filter (Filter   : in Verify_Filter;
                        Request  : in out ASF.Requests.Request'Class;
                        Response : in out ASF.Responses.Response'Class;
                        Chain    : in out ASF.Servlets.Filter_Chain) is
      Key     : constant String := Request.Get_Parameter ("key");
      Manager : AWA.Users.Services.User_Service_Access := AWA.Users.Module.Get_User_Manager;
      User    : AWA.Users.Models.User_Ref;
      Session : AWA.Users.Models.Session_Ref;
   begin
      Log.Info ("Verify access key {0}", Key);

      Manager.Verify_User (Key     => Key,
                           User    => User,
                           IpAddr  => "",
                           Session => Session);

      Set_Session_Principal (Request, User, Session);

      --  Request is authorized, proceed to the next filter.
      ASF.Servlets.Do_Filter (Chain    => Chain,
                              Request  => Request,
                              Response => Response);

   exception
      when AWA.Users.Services.Not_Found =>
         declare
            URI : constant String := To_String (Filter.Invalid_Key_URI);
         begin
            Log.Info ("Invalid access key {0}, redirecting to {1}", Key, URI);
            Response.Send_Redirect (Location => URI);
         end;
   end Do_Filter;

end AWA.Users.Filters;
