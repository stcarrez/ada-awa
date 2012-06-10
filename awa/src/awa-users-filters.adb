-----------------------------------------------------------------------
--  awa-users-filters -- Specific filters for authentication and key verification
--  Copyright (C) 2011, 2012 Stephane Carrez
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

with ASF.Cookies;

with AWA.Users.Services;
with AWA.Users.Modules;

package body AWA.Users.Filters is

   --  The logger
   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("AWA.Users.Filters");


   --  ------------------------------
   --  Set the user principal on the session associated with the ASF request.
   --  ------------------------------
   procedure Set_Session_Principal (Request   : in out ASF.Requests.Request'Class;
                                    Principal : in Principals.Principal_Access) is
      Session   : ASF.Sessions.Session := Request.Get_Session (Create => True);
   begin
      Session.Set_Principal (Principal.all'Access);
   end Set_Session_Principal;

   --  ------------------------------
   --  Initialize the filter and configure the redirection URIs.
   --  ------------------------------
   procedure Initialize (Filter  : in out Auth_Filter;
                         Context : in ASF.Servlets.Servlet_Registry'Class) is
      URI : constant String := Context.Get_Init_Parameter (AUTH_FILTER_REDIRECT_PARAM);
   begin
      Log.Info ("Using login URI: {0}", URI);

      if URI = "" then
         Log.Error ("The login URI is empty.  Redirection to the login page will not work.");
      end if;
      Filter.Login_URI := To_Unbounded_String (URI);
      ASF.Security.Filters.Auth_Filter (Filter).Initialize (Context);
   end Initialize;

   procedure Authenticate (F         : in Auth_Filter;
                           Request   : in out ASF.Requests.Request'Class;
                           Response  : in out ASF.Responses.Response'Class;
                           Session   : in ASF.Sessions.Session;
                           Auth_Id   : in String;
                           Principal : out ASF.Principals.Principal_Access) is
      pragma Unreferenced (F, Session);

      use AWA.Users.Modules;
      use AWA.Users.Services;

      Manager : constant User_Service_Access := AWA.Users.Modules.Get_User_Manager;
      P       : AWA.Users.Principals.Principal_Access;
   begin
      Manager.Authenticate (Cookie  => Auth_Id,
                            Ip_Addr => "",
                            Principal => P);
      Principal := P.all'Access;

      --  Setup a new AID cookie with the new connection session.
      declare
         Cookie : constant String := Manager.Get_Authenticate_Cookie (P.Get_Session_Identifier);
         C      : ASF.Cookies.Cookie := ASF.Cookies.Create (ASF.Security.Filters.AID_COOKIE, Cookie);
      begin
         ASF.Cookies.Set_Path (C, Request.Get_Context_Path);
         ASF.Cookies.Set_Max_Age (C, 15 * 86400);
         Response.Add_Cookie (Cookie => C);
      end;

   exception
      when Not_Found =>
         Principal := null;
   end Authenticate;

   --  ------------------------------
   --  Display or redirects the user to the login page.  This procedure is called when
   --  the user is not authenticated.
   --  ------------------------------
   overriding
   procedure Do_Login (Filter   : in Auth_Filter;
                       Request  : in out ASF.Requests.Request'Class;
                       Response : in out ASF.Responses.Response'Class) is
      URI : constant String := To_String (Filter.Login_URI);
   begin
      Log.Info ("User is not logged, redirecting to {0}", URI);

      if Request.Get_Header ("X-Requested-With") = "" then
         Response.Send_Redirect (Location => URI);
      else
         Response.Send_Error (ASF.Responses.SC_UNAUTHORIZED);
      end if;
   end Do_Login;

   --  ------------------------------
   --  Initialize the filter and configure the redirection URIs.
   --  ------------------------------
   overriding
   procedure Initialize (Filter  : in out Verify_Filter;
                         Context : in ASF.Servlets.Servlet_Registry'Class) is
      URI : constant String := Context.Get_Init_Parameter (VERIFY_FILTER_REDIRECT_PARAM);
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
   overriding
   procedure Do_Filter (Filter   : in Verify_Filter;
                        Request  : in out ASF.Requests.Request'Class;
                        Response : in out ASF.Responses.Response'Class;
                        Chain    : in out ASF.Servlets.Filter_Chain) is
      Key       : constant String := Request.Get_Parameter (PARAM_ACCESS_KEY);
      Manager   : constant Users.Services.User_Service_Access := Users.Modules.Get_User_Manager;
      Principal : AWA.Users.Principals.Principal_Access;
   begin
      Log.Info ("Verify access key {0}", Key);

      Manager.Verify_User (Key       => Key,
                           IpAddr    => "",
                           Principal => Principal);

      Set_Session_Principal (Request, Principal);

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
