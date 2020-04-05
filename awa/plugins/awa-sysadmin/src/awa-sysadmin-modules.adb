-----------------------------------------------------------------------
--  awa-sysadmin-modules -- Module sysadmin
--  Copyright (C) 2019, 2020 Stephane Carrez
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

with AWA.Applications;
with AWA.Modules.Get;
with AWA.Modules.Beans;
with AWA.Sysadmin.Models;
with Util.Log.Loggers;
with ADO.Sessions;
with ADO.Statements;
with ADO.Queries;
with ADO.Utils.Serialize;
with Servlet.Rest;
with Swagger.Servers.Operation;
with AWA.Sysadmin.Beans;
package body AWA.Sysadmin.Modules is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Awa.Sysadmin.Module");

   package Register is new AWA.Modules.Beans (Module => Sysadmin_Module,
                                              Module_Access => Sysadmin_Module_Access);

   procedure List_Users
     (Req     : in out Swagger.Servers.Request'Class;
      Reply   : in out Swagger.Servers.Response'Class;
      Stream  : in out Swagger.Servers.Output_Stream'Class;
      Context : in out Swagger.Servers.Context_Type);

   procedure List_Sessions
     (Req     : in out Swagger.Servers.Request'Class;
      Reply   : in out Swagger.Servers.Response'Class;
      Stream  : in out Swagger.Servers.Output_Stream'Class;
      Context : in out Swagger.Servers.Context_Type);

   procedure List_Jobs
     (Req     : in out Swagger.Servers.Request'Class;
      Reply   : in out Swagger.Servers.Response'Class;
      Stream  : in out Swagger.Servers.Output_Stream'Class;
      Context : in out Swagger.Servers.Context_Type);

   package API_List_Users is
     new Swagger.Servers.Operation (Handler => List_Users,
                                    Method  => Swagger.Servers.GET,
                                    URI     => "/sysadmin/api/v1/users");

   package API_List_Sessions is
     new Swagger.Servers.Operation (Handler => List_Sessions,
                                    Method  => Swagger.Servers.GET,
                                    URI     => "/sysadmin/api/v1/sessions");

   package API_List_Jobs is
     new Swagger.Servers.Operation (Handler => List_Jobs,
                                    Method  => Swagger.Servers.GET,
                                    URI     => "/sysadmin/api/v1/jobs");

   procedure List_Users
     (Req     : in out Swagger.Servers.Request'Class;
      Reply   : in out Swagger.Servers.Response'Class;
      Stream  : in out Swagger.Servers.Output_Stream'Class;
      Context : in out Swagger.Servers.Context_Type) is
      pragma Unreferenced (Req, Reply);

      Module  : constant Sysadmin_Module_Access := Get_Sysadmin_Module;
      Session : constant ADO.Sessions.Session := Module.Get_Session;
      Query   : ADO.Queries.Context;

      Stmt : ADO.Statements.Query_Statement;
   begin
      Log.Info ("List users");

      Query.Set_Query (AWA.Sysadmin.Models.Query_Sysadmin_User_List);
      Stmt := Session.Create_Statement (Query);
      Stmt.Execute;

      Stream.Start_Document;
      ADO.Utils.Serialize.Write_Query (Stream, "", Stmt);
      Stream.End_Document;
      Context.Set_Status (200);
   end List_Users;

   procedure List_Sessions
     (Req     : in out Swagger.Servers.Request'Class;
      Reply   : in out Swagger.Servers.Response'Class;
      Stream  : in out Swagger.Servers.Output_Stream'Class;
      Context : in out Swagger.Servers.Context_Type) is
      pragma Unreferenced (Req, Reply);

      Module  : constant Sysadmin_Module_Access := Get_Sysadmin_Module;
      Session : constant ADO.Sessions.Session := Module.Get_Session;
      Query   : ADO.Queries.Context;

      Stmt : ADO.Statements.Query_Statement;
   begin
      Log.Info ("List sessions");

      Query.Set_Query (AWA.Sysadmin.Models.Query_Sysadmin_Session_List);
      Stmt := Session.Create_Statement (Query);
      Stmt.Execute;

      Stream.Start_Document;
      ADO.Utils.Serialize.Write_Query (Stream, "", Stmt);
      Stream.End_Document;
      Context.Set_Status (200);
   end List_Sessions;

   procedure List_Jobs
     (Req     : in out Swagger.Servers.Request'Class;
      Reply   : in out Swagger.Servers.Response'Class;
      Stream  : in out Swagger.Servers.Output_Stream'Class;
      Context : in out Swagger.Servers.Context_Type) is
      pragma Unreferenced (Req, Reply);

      Module  : constant Sysadmin_Module_Access := Get_Sysadmin_Module;
      Session : constant ADO.Sessions.Session := Module.Get_Session;
      Query   : ADO.Queries.Context;

      Stmt : ADO.Statements.Query_Statement;
   begin
      Log.Info ("List jobs");

      Query.Set_Query (AWA.Sysadmin.Models.Query_Sysadmin_Job_List);
      Stmt := Session.Create_Statement (Query);
      Stmt.Execute;

      Stream.Start_Document;
      ADO.Utils.Serialize.Write_Query (Stream, "", Stmt);
      Stream.End_Document;
      Context.Set_Status (200);
   end List_Jobs;

   --  ------------------------------
   --  Initialize the sysadmin module.
   --  ------------------------------
   overriding
   procedure Initialize (Plugin : in out Sysadmin_Module;
                         App    : in AWA.Modules.Application_Access;
                         Props  : in ASF.Applications.Config) is
   begin
      Log.Info ("Initializing the sysadmin module");

      App.Add_Servlet ("sysadmin", Plugin.API_Servlet'Unchecked_Access);
      App.Add_Filter ("sysadmin-filter", Plugin.API_Filter'Unchecked_Access);

      Register.Register (Plugin  => Plugin,
                         Name    => "AWA.Sysadmin.Beans.Authenticate_Bean",
                         Handler => AWA.Sysadmin.Beans.Create_Authenticate_Bean'Access);

      App.Add_Mapping (Name => "sysadmin", Pattern => "/sysadmin/api/*");

      AWA.Modules.Module (Plugin).Initialize (App, Props);

      Servlet.Rest.Register (App.all, API_List_Users.Definition);
      Servlet.Rest.Register (App.all, API_List_Sessions.Definition);
      Servlet.Rest.Register (App.all, API_List_Jobs.Definition);
   end Initialize;

   --  ------------------------------
   --  Get the sysadmin module.
   --  ------------------------------
   function Get_Sysadmin_Module return Sysadmin_Module_Access is
      function Get is new AWA.Modules.Get (Sysadmin_Module, Sysadmin_Module_Access, NAME);
   begin
      return Get;
   end Get_Sysadmin_Module;

end AWA.Sysadmin.Modules;
