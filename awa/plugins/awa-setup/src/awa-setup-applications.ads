-----------------------------------------------------------------------
--  awa-setup-applications -- Setup and installation
--  Copyright (C) 2016, 2017, 2018, 2019, 2020, 2022, 2023 Stephane Carrez
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
with ASF.Applications.Main;
with ASF.Servlets.Faces;
with Servlet.Core.Files;
with ASF.Requests;
with ASF.Responses;
with ASF.Server;
with ADO.Connections;
with Util.Beans.Objects;
with Util.Beans.Basic;
with Util.Beans.Methods;

--  == Setup Procedure Instantiation==
--  The setup process is managed by the *Configure* generic procedure.
--  The procedure must be instantiated with the application class type and
--  the application initialize procedure.
--
--     procedure Setup is
--        new AWA.Setup.Applications.Configure (MyApp.Application'Class,
--                                              MyApp.Application_Access,
--                                              MyApp.Initialize);
--
--  == Setup Operation ==
--  The *Setup* instantiated operation must then be called with the web container.
--  The web container is started first and the *Setup* procedure gets as parameter
--  the web container, the application instance to configure, the application name
--  and the application context path.
--
--    Setup (WS, App, "atlas", MyApp.CONTEXT_PATH)
--
--  The operation will install the setup application to handle the setup actions.
--  Through the setup actions, the installer will be able to:
--
--  * Configure the database (MySQL or SQLite),
--  * Configure the Google+ and Facebook OAuth authentication keys,
--  * Configure the application name,
--  * Configure the mail parameters to be able to send email.
--
--  After the setup and configure is finished, the file <tt>.initialized</tt>
--  is created in the application directory to indicate the application is
--  configured.  The next time the *Setup* operation is called, the installation
--  process will be skipped.
--
--  To run again the installation, remove manually the <tt>.initialized</tt> file.
package AWA.Setup.Applications is

   package UBO renames Util.Beans.Objects;

   Empty : constant Util.Beans.Objects.Object := Util.Beans.Objects.To_Object (String '(""));

   --  The <b>Servlet</b> represents the component that will handle
   --  an HTTP request received by the server.
   type Redirect_Servlet is new Servlet.Core.Servlet with null record;

   overriding
   procedure Do_Get (Server   : in Redirect_Servlet;
                     Request  : in out ASF.Requests.Request'Class;
                     Response : in out ASF.Responses.Response'Class);

   --  The configuration state starts in the `CONFIGURING` state and moves to the
   --  `STARTING` state after the application is configured and it is started.
   --  Once the application is initialized and registered in the server container,
   --  the state is changed to `READY`.
   type Configure_State is (CONFIGURING, STARTING, READY);

   --  Maintains the state of the configuration between the main task and
   --  the http configuration requests.
   protected type State is
      --  Wait until the configuration is finished.
      entry Wait_Configuring;

      --  Wait until the server application is initialized and ready.
      entry Wait_Ready;

      --  Set the configuration state.
      procedure Set (V : in Configure_State);
   private
      Value : Configure_State := CONFIGURING;
   end State;

   type Application is new ASF.Applications.Main.Application and Util.Beans.Basic.Bean
     and Util.Beans.Methods.Method_Bean with record
      Faces       : aliased ASF.Servlets.Faces.Faces_Servlet;
      Files       : aliased Servlet.Core.Files.File_Servlet;
      Redirect    : aliased Redirect_Servlet;
      Config      : ASF.Applications.Config;
      Changed     : ASF.Applications.Config;
      Factory     : ASF.Applications.Main.Application_Factory;
      Path        : Ada.Strings.Unbounded.Unbounded_String;
      Name        : Ada.Strings.Unbounded.Unbounded_String;
      Database    : ADO.Connections.Configuration;
      Driver      : Util.Beans.Objects.Object;
      Result      : Util.Beans.Objects.Object;
      Root_User   : Util.Beans.Objects.Object := Empty;
      Root_Passwd : Util.Beans.Objects.Object := Empty;
      Db_Host     : Util.Beans.Objects.Object;
      Db_Port     : Util.Beans.Objects.Object;
      Has_Error   : Boolean := False;
      Status      : State;
   end record;

   --  Get the value identified by the name.
   overriding
   function Get_Value (From : in Application;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the value identified by the name.
   overriding
   procedure Set_Value (From  : in out Application;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   --  Get the database connection string to be used by the application.
   function Get_Database_URL (From : in Application) return String;

   --  Get the command to configure the database.
   function Get_Configure_Command (From : in Application) return String;

   --  Configure the database.
   procedure Configure_Database (From    : in out Application;
                                 Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   --  Validate the database configuration parameters.
   procedure Validate (From : in out Application);

   --  Save the configuration.
   procedure Save (From    : in out Application;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   --  Finish the setup to start the application.
   procedure Start (From    : in out Application;
                    Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   --  Finish the setup and wait for the application to be started.
   procedure Finish (From    : in out Application;
                     Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   --  This bean provides some methods that can be used in a Method_Expression
   overriding
   function Get_Method_Bindings (From : in Application)
                                 return Util.Beans.Methods.Method_Binding_Array_Access;

   --  Enter in the application setup
   procedure Setup (App    : in out Application;
                    Config : in String;
                    Server : in out ASF.Server.Container'Class);

   --  Configure the application by using the setup application, allowing
   --  the administrator to setup the application database, define the application
   --  admin parameters.  After the configuration is done, register the
   --  application in the server container and start it.
   generic
      type Application_Type (<>) is new ASF.Servlets.Servlet_Registry with private;
      type Application_Access is access all Application_Type'Class;
      with procedure Initialize (App    : in Application_Access;
                                 Config : in ASF.Applications.Config);
   procedure Configure (Server : in out ASF.Server.Container'Class;
                        App    : in Application_Access;
                        Config : in String;
                        URI    : in String);

end AWA.Setup.Applications;
