-----------------------------------------------------------------------
--  awa-setup -- Setup and installation
--  Copyright (C) 2016 Stephane Carrez
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
with ASF.Servlets.Files;
with ASF.Requests;
with ASF.Responses;
with ASF.Server;
with Util.Beans.Objects;
with Util.Beans.Basic;
with Util.Beans.Methods;
with ADO.Drivers.Connections;
package AWA.Setup.Applications is

   --  The <b>Servlet</b> represents the component that will handle
   --  an HTTP request received by the server.
   type Redirect_Servlet is new ASF.Servlets.Servlet with null record;

   overriding
   procedure Do_Get (Server   : in Redirect_Servlet;
                     Request  : in out ASF.Requests.Request'Class;
                     Response : in out ASF.Responses.Response'Class);

   type Application is new ASF.Applications.Main.Application and Util.Beans.Basic.Bean
     and Util.Beans.Methods.Method_Bean with record
      Faces    : aliased ASF.Servlets.Faces.Faces_Servlet;
      Files    : aliased ASF.Servlets.Files.File_Servlet;
      Redirect : aliased Redirect_Servlet;
      Config   : ASF.Applications.Config;
      Changed  : ASF.Applications.Config;
      Factory  : ASF.Applications.Main.Application_Factory;
      Path     : Ada.Strings.Unbounded.Unbounded_String;
      Database : ADO.Drivers.Connections.Configuration;
      Done     : Boolean := False;
      pragma Atomic (Done);
      pragma Volatile (Done);
   end record;

   --  Get the value identified by the name.
   function Get_Value (From : in Application;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the value identified by the name.
   procedure Set_Value (From  : in out Application;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   --  Get the database connection string to be used by the application.
   function Get_Database_URL (From : in Application) return String;

   --  Configure the database.
   procedure Configure_Database (From    : in out Application;
                                 Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   --  Save the configuration.
   procedure Save (From    : in out Application;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   --  Finish the setup and exit the setup.
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

end AWA.Setup.Applications;
