-----------------------------------------------------------------------
--  awa-users-filters -- Specific filters for authentication
--  Copyright (C) 2011, 2012, 2013, 2015, 2019, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Log.Loggers;

with ASF.Applications.Main;
with Util.Http.Cookies;

with AWA.Services.Contexts;
with AWA.Users.Services;
with AWA.Users.Modules;
with AWA.Users.Principals;

package body AWA.Users.Filters is

   use Util.Http;

   --  The logger
   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("AWA.Users.Filters");

   --  ------------------------------
   --  Get the redirection URL from the redirect cookie.
   --  ------------------------------
   function Get_Redirect_Cookie (Request : in Servlet.Requests.Request'Class) return String is
      Cookies : constant Util.Http.Cookies.Cookie_Array := Request.Get_Cookies;
   begin
      for I in Cookies'Range loop
         if Util.Http.Cookies.Get_Name (Cookies (I)) = REDIRECT_COOKIE then
            return Util.Http.Cookies.Get_Value (Cookies (I));
         end if;
      end loop;
      return "";
   end Get_Redirect_Cookie;

   --  ------------------------------
   --  Clear the redirect cookie in the response.
   --  ------------------------------
   procedure Clear_Redirect_Cookie (Request : in Servlet.Requests.Request'Class;
                                    Response : in out Servlet.Responses.Response'Class) is
      C : Util.Http.Cookies.Cookie := Util.Http.Cookies.Create (REDIRECT_COOKIE, "");
   begin
      Util.Http.Cookies.Set_Path (C, Request.Get_Context_Path);
      Util.Http.Cookies.Set_Max_Age (C, 0);
      Response.Add_Cookie (Cookie => C);
   end Clear_Redirect_Cookie;

   --  ------------------------------
   --  Initialize the filter and configure the redirection URIs.
   --  ------------------------------
   overriding
   procedure Initialize (Filter  : in out Auth_Filter;
                         Config  : in Servlet.Core.Filter_Config) is
      use ASF.Applications.Main;
      Context : constant Servlet.Core.Servlet_Registry_Access
        := Servlet.Core.Get_Servlet_Context (Config);
      URI : constant String
        := Servlet.Core.Get_Init_Parameter (Config,
                                            AUTH_FILTER_REDIRECT_PARAM);
   begin
      Log.Info ("Using login URI: {0}", URI);

      if URI = "" then
         Log.Error ("The login URI is empty.  Redirection to the login page will not work.");
      end if;
      Filter.Login_URI := To_Unbounded_String (URI);
      Servlet.Security.Filters.Auth_Filter (Filter).Initialize (Config);
      if Context.all in Application'Class then
         Filter.Application := AWA.Applications.Application'Class (Context.all)'Access;
         Filter.Set_Permission_Manager (Application'Class (Context.all).Get_Security_Manager);
      end if;
   end Initialize;

   overriding
   procedure Authenticate (F         : in Auth_Filter;
                           Request   : in out Servlet.Requests.Request'Class;
                           Response  : in out Servlet.Responses.Response'Class;
                           Session   : in Servlet.Sessions.Session;
                           Auth_Id   : in String;
                           Principal : out ASF.Principals.Principal_Access) is
      pragma Unreferenced (Session);

      use AWA.Users.Services;

      Context     : aliased AWA.Services.Contexts.Service_Context;
      Manager     : User_Service_Access;
      P           : AWA.Users.Principals.Principal_Access;
   begin
      --  Setup the service context.
      Context.Set_Context (F.Application, null);

      Manager := AWA.Users.Modules.Get_User_Manager;
      Manager.Authenticate (Cookie  => Auth_Id,
                            Ip_Addr => "",
                            Principal => P);
      Principal := P.all'Access;

      --  Setup a new AID cookie with the new connection session.
      declare
         Cookie : constant String := Manager.Get_Authenticate_Cookie (P.Get_Session_Identifier);
         C      : Cookies.Cookie := Cookies.Create (Servlet.Security.Filters.AID_COOKIE,
                                                    Cookie);
      begin
         Cookies.Set_Path (C, Request.Get_Context_Path);
         Cookies.Set_Max_Age (C, 15 * 86400);
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
                       Request  : in out Servlet.Requests.Request'Class;
                       Response : in out Servlet.Responses.Response'Class) is
      Login_URI : constant String := To_String (Filter.Login_URI);
      Context   : constant String := Request.Get_Context_Path;
      Path      : constant String := Request.Get_Servlet_Path;
      URL       : constant String := Context & Path & Request.Get_Path_Info;
      C         : Cookies.Cookie := Cookies.Create (REDIRECT_COOKIE, URL);
   begin
      Log.Info ("User is not logged, redirecting to {0}", Login_URI);

      Cookies.Set_Path (C, Request.Get_Context_Path);
      Cookies.Set_Max_Age (C, 86400);
      Response.Add_Cookie (Cookie => C);
      if Request.Get_Header ("X-Requested-With") = "" then
         Response.Send_Redirect (Location => Login_URI);
      else
         Response.Send_Error (Servlet.Responses.SC_UNAUTHORIZED);
      end if;
   end Do_Login;

end AWA.Users.Filters;
