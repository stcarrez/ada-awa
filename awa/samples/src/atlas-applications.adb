-----------------------------------------------------------------------
--  atlas -- atlas applications
-----------------------------------------------------------------------
--  Copyright (C) 2012, 2013 Stephane Carrez
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

with Ada.IO_Exceptions;
with GNAT.MD5;

with Util.Log.Loggers;
with Util.Properties;
with Util.Beans.Basic;
with Util.Strings.Transforms;

with EL.Functions;

with ASF.Applications;
with ASF.Applications.Main;

with ADO.Queries;
with ADO.Sessions;

with AWA.Applications.Factory;
with AWA.Services.Contexts;
with Atlas.Applications.Models;

--  with Atlas.XXX.Module;
package body Atlas.Applications is

   package ASC renames AWA.Services.Contexts;

   use AWA.Applications;

   type User_Stat_Info_Access is access all Atlas.Applications.Models.User_Stat_Info;

   Log     : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Atlas");

   procedure Set_Functions (Mapper : in out EL.Functions.Function_Mapper'Class);
   function Create_User_Stat_Bean return Util.Beans.Basic.Readonly_Bean_Access;

   --  ------------------------------
   --  Create the user statistics bean which indicates what feature the user has used.
   --  ------------------------------
   function Create_User_Stat_Bean return Util.Beans.Basic.Readonly_Bean_Access is
      use type ASC.Service_Context_Access;

      Ctx    : constant ASC.Service_Context_Access := ASC.Current;
      Result : User_Stat_Info_Access;
   begin
      if Ctx /= null then
         declare
            List    : Atlas.Applications.Models.User_Stat_Info_List_Bean;
            Session : ADO.Sessions.Session := ASC.Get_Session (Ctx);
            User    : constant ADO.Identifier := Ctx.Get_User_Identifier;
            Query   : ADO.Queries.Context;
         begin
            Query.Set_Query (Atlas.Applications.Models.Query_User_Stat);
            Query.Bind_Param ("user_id", User);
            Atlas.Applications.Models.List (List, Session, Query);

            Result := new Atlas.Applications.Models.User_Stat_Info;
            Result.all := List.List.Element (0);
         end;
      else
         Result := new Atlas.Applications.Models.User_Stat_Info;
         Result.Post_Count     := 0;
         Result.Document_Count := 0;
         Result.Question_Count := 0;
         Result.Answer_Count   := 0;
      end if;
      return Result.all'Access;
   end Create_User_Stat_Bean;

   --  ------------------------------
   --  Given an Email address, return the Gravatar link to the user image.
   --  (See http://en.gravatar.com/site/implement/hash/ and
   --  http://en.gravatar.com/site/implement/images/)
   --  ------------------------------
   function Get_Gravatar_Link (Email : in String) return String is
      E : constant String := Util.Strings.Transforms.To_Lower_Case (Email);
      C : constant GNAT.MD5.Message_Digest := GNAT.MD5.Digest (E);
   begin
      return "http://www.gravatar.com/avatar/" & C;
   end Get_Gravatar_Link;

   --  ------------------------------
   --  EL function to convert an Email address to a Gravatar image.
   --  ------------------------------
   function To_Gravatar_Link (Email : in Util.Beans.Objects.Object)
                              return Util.Beans.Objects.Object is
      Link : constant String := Get_Gravatar_Link (Util.Beans.Objects.To_String (Email));
   begin
      return Util.Beans.Objects.To_Object (Link);
   end To_Gravatar_Link;

   procedure Set_Functions (Mapper : in out EL.Functions.Function_Mapper'Class) is
   begin
      Mapper.Set_Function (Name      => "gravatar",
                           Namespace => ATLAS_NS_URI,
                           Func      => To_Gravatar_Link'Access);
   end Set_Functions;

   --  ------------------------------
   --  Initialize the application:
   --  <ul>
   --     <li>Register the servlets and filters.
   --     <li>Register the application modules.
   --     <li>Define the servlet and filter mappings.
   --  </ul>
   --  ------------------------------
   procedure Initialize (App : in Application_Access) is
      Fact  : AWA.Applications.Factory.Application_Factory;
      C     : ASF.Applications.Config;
   begin
      App.Self := App;
      begin
         C.Load_Properties ("atlas.properties");
         Util.Log.Loggers.Initialize (Util.Properties.Manager (C));

      exception
         when Ada.IO_Exceptions.Name_Error =>
            Log.Error ("Cannot read application configuration file {0}", CONFIG_PATH);
      end;
      App.Initialize (C, Fact);

      App.Set_Global ("contextPath", CONTEXT_PATH);
   end Initialize;

   --  ------------------------------
   --  Initialize the ASF components provided by the application.
   --  This procedure is called by <b>Initialize</b>.
   --  It should register the component factories used by the application.
   --  ------------------------------
   overriding
   procedure Initialize_Components (App : in out Application) is
      procedure Register is
         new ASF.Applications.Main.Register_Functions (Set_Functions);
   begin
      Register (App);
      AWA.Applications.Application (App).Initialize_Components;
      App.Add_Converter (Name      => "smartDateConverter",
                         Converter => App.Self.Rel_Date_Converter'Access);
      App.Add_Converter (Name      => "sizeConverter",
                         Converter => App.Self.Size_Converter'Access);

      App.Register_Class (Name => "Atlas.Applications.User_Stat_Bean",
                          Handler => Create_User_Stat_Bean'Access);
   end Initialize_Components;

   --  ------------------------------
   --  Initialize the servlets provided by the application.
   --  This procedure is called by <b>Initialize</b>.
   --  It should register the application servlets.
   --  ------------------------------
   overriding
   procedure Initialize_Servlets (App : in out Application) is
   begin
      Log.Info ("Initializing application servlets...");

      AWA.Applications.Application (App).Initialize_Servlets;
      App.Add_Servlet (Name => "faces", Server => App.Self.Faces'Access);
      App.Add_Servlet (Name => "files", Server => App.Self.Files'Access);
      App.Add_Servlet (Name => "ajax", Server => App.Self.Ajax'Access);
      App.Add_Servlet (Name => "measures", Server => App.Self.Measures'Access);
      App.Add_Servlet (Name => "auth", Server => App.Self.Auth'Access);
      App.Add_Servlet (Name => "verify-auth", Server => App.Self.Verify_Auth'Access);
   end Initialize_Servlets;

   --  ------------------------------
   --  Initialize the filters provided by the application.
   --  This procedure is called by <b>Initialize</b>.
   --  It should register the application filters.
   --  ------------------------------
   overriding
   procedure Initialize_Filters (App : in out Application) is
   begin
      Log.Info ("Initializing application filters...");

      AWA.Applications.Application (App).Initialize_Filters;
      App.Add_Filter (Name => "dump", Filter => App.Dump'Access);
      App.Add_Filter (Name => "measures", Filter => App.Measures'Access);
      App.Add_Filter (Name => "service", Filter => App.Service_Filter'Access);
   end Initialize_Filters;

   --  ------------------------------
   --  Initialize the AWA modules provided by the application.
   --  This procedure is called by <b>Initialize</b>.
   --  It should register the modules used by the application.
   --  ------------------------------
   overriding
   procedure Initialize_Modules (App : in out Application) is
   begin
      Log.Info ("Initializing application modules...");

      Register (App    => App.Self.all'Access,
                Name   => AWA.Users.Modules.NAME,
                URI    => "user",
                Module => App.User_Module'Access);

      Register (App    => App.Self.all'Access,
                Name   => AWA.Workspaces.Modules.NAME,
                URI    => "workspaces",
                Module => App.Workspace_Module'Access);

      Register (App    => App.Self.all'Access,
                Name   => AWA.Mail.Modules.NAME,
                URI    => "mail",
                Module => App.Mail_Module'Access);

      Register (App    => App.Self.all'Access,
                Name   => AWA.Tags.Modules.NAME,
                URI    => "tags",
                Module => App.Tag_Module'Access);

      Register (App    => App.Self.all'Access,
                Name   => AWA.Blogs.Modules.NAME,
                URI    => "blogs",
                Module => App.Blog_Module'Access);

      Register (App    => App.Self.all'Access,
                Name   => AWA.Storages.Modules.NAME,
                URI    => "storages",
                Module => App.Storage_Module'Access);

      Register (App    => App.Self.all'Access,
                Name   => AWA.Images.Modules.NAME,
                URI    => "images",
                Module => App.Image_Module'Access);

      Register (App    => App.Self.all'Access,
                Name   => AWA.Votes.Modules.NAME,
                URI    => "votes",
                Module => App.Vote_Module'Access);

      Register (App    => App.Self.all'Access,
                Name   => AWA.Questions.Modules.NAME,
                URI    => "questions",
                Module => App.Question_Module'Access);

      Register (App    => App.Self.all'Access,
                Name   => Atlas.Microblog.Modules.NAME,
                URI    => "microblog",
                Module => App.Microblog_Module'Access);
   end Initialize_Modules;

end Atlas.Applications;
