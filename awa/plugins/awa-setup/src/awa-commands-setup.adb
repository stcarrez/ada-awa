-----------------------------------------------------------------------
--  awa-commands-setup -- Setup command to start and configure the application
--  Copyright (C) 2020 Stephane Carrez
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

with GNAT.Command_Line;
with AWA.Applications;
with AWA.Setup.Applications;
with Servlet.Core;

package body AWA.Commands.Setup is

   use AWA.Applications;

   package Command_Drivers renames Start_Command.Command_Drivers;

   overriding
   procedure Execute (Command   : in out Command_Type;
                      Name      : in String;
                      Args      : in Argument_List'Class;
                      Context   : in out Context_Type) is
      procedure Find_Application (Name : in String);
      procedure Enable_Application (URI : in String;
                                    App : in Servlet.Core.Servlet_Registry_Access);

      Count    : Natural := 0;
      Selected : Application_Access;

      procedure Find_Application (Name : in String) is
         procedure Find (URI : in String;
                         App : in Servlet.Core.Servlet_Registry_Access);

         procedure Find (URI : in String;
                         App : in Servlet.Core.Servlet_Registry_Access) is
         begin
            App.Disable;
            if URI (URI'First + 1 .. URI'Last) = Name then
               if App.all in Application'Class then
                  Selected := Application'Class (App.all)'Unchecked_Access;
                  Count := Count + 1;
               end if;
            end if;
         end Find;

      begin
         Command_Drivers.WS.Iterate (Find'Access);
      end Find_Application;

      procedure Enable_Application (URI : in String;
                                    App : in Servlet.Core.Servlet_Registry_Access) is
      begin
         App.Enable;
         App.Start;
      end Enable_Application;

      S : aliased AWA.Setup.Applications.Application;
   begin
      if Args.Get_Count /= 1 then
         Context.Console.Notice (N_ERROR, -("Missing application name"));
         return;
      end if;

      declare
         Name : constant String := Args.Get_Argument (1);
      begin
         Find_Application (Name);
         if Count /= 1 then
            Context.Console.Notice (N_ERROR, -("No application found"));
            return;
         end if;

         Command.Configure_Server (Context);
         Command.Start_Server (Context);
         S.Setup (Name, Command_Drivers.WS);
         Command_Drivers.WS.Remove_Application (S'Unchecked_Access);
         Command.Configure_Applications (Context);
         Command_Drivers.WS.Iterate (Enable_Application'Access);
         Command.Wait_Server (Context);
      end;
   end Execute;

   --  Setup the command before parsing the arguments and executing it.
   overriding
   procedure Setup (Command : in out Command_Type;
                    Config  : in out GNAT.Command_Line.Command_Line_Configuration;
                    Context : in out Context_Type) is
   begin
      Start_Command.Command_Type (Command).Setup (Config, Context);
   end Setup;

begin
   Command_Drivers.Driver.Add_Command ("setup",
                                       -("start the web server and setup the application"),
                                       Command'Access);
end AWA.Commands.Setup;
