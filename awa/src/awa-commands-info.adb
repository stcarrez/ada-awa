-----------------------------------------------------------------------
--  akt-commands-info -- Info command to describe the current configuration
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
package body AWA.Commands.Info is

   use type AWA.Modules.Module_Access;

   --  ------------------------------
   --  Print the configuration identified by the given name.
   --  ------------------------------
   procedure Print (Command     : in out Command_Type;
                    Name        : in String;
                    Value       : in String;
                    Default     : in String;
                    Context     : in out Context_Type) is
      pragma Unreferenced (Default);

      Pos   : Natural;
   begin
      Context.Console.Start_Row;
      Context.Console.Print_Field (1, Name);
      Pos := Value'First;
      while Pos <= Value'Last loop
         if Value'Last - Pos > Command.Value_Length then
            Context.Console.Print_Field (2, Value (Pos .. Pos + Command.Value_Length - 1));
            Pos := Pos + Command.Value_Length;
            Context.Console.End_Row;
            Context.Console.Start_Row;
            Context.Console.Print_Field (1, "");
         else
            Context.Console.Print_Field (2, Value (Pos .. Value'Last));
            Pos := Pos + Value'Length;
         end if;
      end loop;
      Context.Console.End_Row;
   end Print;

   procedure Print (Command     : in out Command_Type;
                    Application : in out AWA.Applications.Application'Class;
                    Name        : in String;
                    Default     : in String;
                    Context     : in out Context_Type) is
      Value : constant String := Application.Get_Init_Parameter (Name, Default);
   begin
      Command.Print (Name, Value, Default, Context);
   end Print;

   procedure Print (Command     : in out Command_Type;
                    Module      : in AWA.Modules.Module'Class;
                    Name        : in String;
                    Default     : in String;
                    Context     : in out Context_Type) is
      Value : constant String := Module.Get_Config (Name, Default);
   begin
      Command.Print (Module.Get_Name & "." & Name, Value, Default, Context);
   end Print;

   --  ------------------------------
   --  Print the configuration about the application.
   --  ------------------------------
   overriding
   procedure Execute (Command     : in out Command_Type;
                      Application : in out AWA.Applications.Application'Class;
                      Args        : in Argument_List'Class;
                      Context     : in out Context_Type) is
      pragma Unreferenced (Args);

      Module : AWA.Modules.Module_Access;
   begin
      if Command.Long_List then
         Command.Value_Length := Natural'Last;
      end if;
      Application.Load_Bundle (Name   => "commands",
                               Locale => "en",
                               Bundle => Command.Bundle);

      Context.Console.Start_Title;
      Context.Console.Print_Title (1, "", 30);
      Context.Console.Print_Title (2, "", Command.Value_Length);
      Context.Console.End_Title;
      Context.Console.Notice (N_INFO, "Database configuration");
      Context.Console.Notice (N_INFO, "----------------------");

      Command.Print (Application, "database", "", Context);
      Command.Print (Application, "ado.queries.paths", "", Context);
      Command.Print (Application, "ado.queries.load", "", Context);
      Command.Print (Application, "ado.drivers.load", "", Context);

      Context.Console.Start_Title;
      Context.Console.Print_Title (1, "", 30);
      Context.Console.Print_Title (2, "", Command.Value_Length);
      Context.Console.End_Title;
      Context.Console.Notice (N_INFO, "Server faces configuration");
      Context.Console.Notice (N_INFO, "--------------------------");

      Command.Print (Application, "view.dir",
                     ASF.Applications.DEF_VIEW_DIR, Context);
      Command.Print (Application, "view.escape_unknown_tags",
                     ASF.Applications.DEF_ESCAPE_UNKNOWN_TAGS, Context);
      Command.Print (Application, "view.ext",
                     ASF.Applications.DEF_VIEW_EXT, Context);
      Command.Print (Application, "view.file_ext",
                     ASF.Applications.DEF_VIEW_FILE_EXT, Context);
      Command.Print (Application, "view.ignore_spaces",
                     ASF.Applications.DEF_IGNORE_WHITE_SPACES, Context);
      Command.Print (Application, "view.ignore_empty_lines",
                     ASF.Applications.DEF_IGNORE_EMPTY_LINES, Context);
      Command.Print (Application, "view.static.dir",
                     ASF.Applications.DEF_STATIC_DIR, Context);
      Command.Print (Application, "bundle.dir", "bundles", Context);

      Context.Console.Start_Title;
      Context.Console.Print_Title (1, "", 30);
      Context.Console.Print_Title (2, "", Command.Value_Length);
      Context.Console.End_Title;
      Context.Console.Notice (N_INFO, "AWA Application");
      Context.Console.Notice (N_INFO, "--------------------------");
      Command.Print (Application, "app_name", "", Context);
      Command.Print (Application, "app_search_dirs", ".", Context);
      Command.Print (Application, "app.modules.dir", "", Context);
      Command.Print (Application, "app_url_base", "", Context);
      Command.Print (Application, "awa_url_base", "", Context);
      Command.Print (Application, "awa_url_host", "", Context);
      Command.Print (Application, "awa_url_port", "", Context);
      Command.Print (Application, "awa_url_scheme", "", Context);
      Command.Print (Application, "app.config", "awa.xml", Context);
      Command.Print (Application, "app.config.plugins", "", Context);
      Command.Print (Application, "contextPath", "", Context);
      Command.Print (Application, "awa_dispatcher_count", "", Context);
      Command.Print (Application, "awa_dispatcher_priority", "", Context);

      Context.Console.Start_Title;
      Context.Console.Print_Title (1, "", 30);
      Context.Console.Print_Title (2, "", Command.Value_Length);
      Context.Console.End_Title;
      Context.Console.Notice (N_INFO, "Users Module");
      Context.Console.Notice (N_INFO, "------------");
      Command.Print (Application, "openid.realm", "", Context);
      Command.Print (Application, "openid.callback_url", "", Context);
      Command.Print (Application, "openid.success_url", "", Context);
      Command.Print (Application, "auth.url.orange", "", Context);
      Command.Print (Application, "auth.provider.oranger", "", Context);
      Command.Print (Application, "auth.url.yahoo", "", Context);
      Command.Print (Application, "auth.provider.yahoo", "", Context);
      Command.Print (Application, "auth.url.google", "", Context);
      Command.Print (Application, "auth.provider.google", "", Context);
      Command.Print (Application, "auth.url.facebook", "", Context);
      Command.Print (Application, "auth.provider.facebook", "", Context);
      Command.Print (Application, "auth.url.google-plus", "", Context);
      Command.Print (Application, "auth.provider.google-plus", "", Context);
      Command.Print (Application, "facebook.callback_url", "", Context);
      Command.Print (Application, "facebook.request_url", "", Context);
      Command.Print (Application, "facebook.scope", "", Context);
      Command.Print (Application, "facebook.client_id", "", Context);
      Command.Print (Application, "facebook.secret", "", Context);
      Command.Print (Application, "google-plus.issuer", "", Context);
      Command.Print (Application, "google-plus.callback_url", "", Context);
      Command.Print (Application, "google-plus.request_url", "", Context);
      Command.Print (Application, "google-plus.scope", "", Context);
      Command.Print (Application, "google-plus.client_id", "", Context);
      Command.Print (Application, "google-plus.secret", "", Context);
      Command.Print (Application, "auth-filter.redirect", "", Context);
      Command.Print (Application, "verify-filter.redirect", "", Context);

      Context.Console.Start_Title;
      Context.Console.Print_Title (1, "", 30);
      Context.Console.Print_Title (2, "", Command.Value_Length);
      Context.Console.End_Title;
      Context.Console.Notice (N_INFO, "Mail Module");
      Context.Console.Notice (N_INFO, "-----------");
      Command.Print (Application, "mail.smtp.host", "", Context);
      Command.Print (Application, "mail.smtp.port", "", Context);
      Command.Print (Application, "mail.smtp.enable", "true", Context);
      Command.Print (Application, "mail.mailer", "smtp", Context);
      Command.Print (Application, "mail.file.maildir", "mail", Context);
      Command.Print (Application, "app_mail_name", "", Context);
      Command.Print (Application, "app_mail_from", "", Context);

      Module := Application.Find_Module ("workspaces");
      if Module /= null then
         Context.Console.Start_Title;
         Context.Console.Print_Title (1, "", 30);
         Context.Console.Print_Title (2, "", Command.Value_Length);
         Context.Console.End_Title;
         Context.Console.Notice (N_INFO, "Workspace Module");
         Context.Console.Notice (N_INFO, "----------------");
         Command.Print (Module.all, "permissions_list", "", Context);
         Command.Print (Module.all, "allow_workspace_create", "", Context);
      end if;

      Module := Application.Find_Module ("storages");
      if Module /= null then
         Context.Console.Start_Title;
         Context.Console.Print_Title (1, "", 30);
         Context.Console.Print_Title (2, "", Command.Value_Length);
         Context.Console.End_Title;
         Context.Console.Notice (N_INFO, "Storage Module");
         Context.Console.Notice (N_INFO, "--------------------------");
         Command.Print (Module.all, "database_max_size", "100000", Context);
         Command.Print (Module.all, "storage_root", "storage", Context);
         Command.Print (Module.all, "tmp_storage_root", "tmp", Context);
         Module := Application.Find_Module ("images");
         if Module /= null then
            Command.Print (Module.all, "thumbnail_command", "", Context);
         end if;
      end if;

      Module := Application.Find_Module ("wikis");
      if Module /= null then
         Context.Console.Start_Title;
         Context.Console.Print_Title (1, "", 30);
         Context.Console.Print_Title (2, "", Command.Value_Length);
         Context.Console.End_Title;
         Context.Console.Notice (N_INFO, "Wiki Module");
         Context.Console.Notice (N_INFO, "-----------");
         Command.Print (Module.all, "image_prefix", "", Context);
         Command.Print (Module.all, "page_prefix", "", Context);
         Command.Print (Module.all, "wiki_copy_list", "", Context);
         Module := Application.Find_Module ("wiki_previews");
         if Module /= null then
            Command.Print (Module.all, "wiki_preview_tmp", "tmp", Context);
            Command.Print (Module.all, "wiki_preview_dir", "web/preview", Context);
            Command.Print (Module.all, "wiki_preview_template", "", Context);
            Command.Print (Module.all, "wiki_preview_html", "", Context);
            Command.Print (Module.all, "wiki_preview_command", "", Context);
         end if;
      end if;

      Module := Application.Find_Module ("counters");
      if Module /= null then
         Context.Console.Start_Title;
         Context.Console.Print_Title (1, "", 30);
         Context.Console.Print_Title (2, "", Command.Value_Length);
         Context.Console.End_Title;
         Context.Console.Notice (N_INFO, "Counter Module");
         Context.Console.Notice (N_INFO, "--------------");
         Command.Print (Module.all, "counter_age_limit", "300", Context);
         Command.Print (Module.all, "counter_limit", "1000", Context);
      end if;
   end Execute;

   --  ------------------------------
   --  Setup the command before parsing the arguments and executing it.
   --  ------------------------------
   overriding
   procedure Setup (Command : in out Command_Type;
                    Config  : in out GNAT.Command_Line.Command_Line_Configuration;
                    Context : in out Context_Type) is
   begin
      GC.Set_Usage (Config => Config,
                    Usage  => Command.Get_Name & " [arguments]",
                    Help   => Command.Get_Description);
      Command_Drivers.Application_Command_Type (Command).Setup (Config, Context);
      GC.Define_Switch (Config => Config,
                        Output => Command.Long_List'Access,
                        Switch => "-l",
                        Long_Switch => "--long-lines",
                        Help   => -("Use long lines to print configuration values"));
   end Setup;

   --  ------------------------------
   --  Write the help associated with the command.
   --  ------------------------------
   overriding
   procedure Help (Command   : in out Command_Type;
                   Name      : in String;
                   Context   : in out Context_Type) is
      pragma Unreferenced (Command, Context);
   begin
      null;
   end Help;

begin
   Command_Drivers.Driver.Add_Command ("info",
                                       -("report configuration information"),
                                       Command'Access);
end AWA.Commands.Info;
