-----------------------------------------------------------------------
--  awa-wikis-modules -- Module wikis
--  Copyright (C) 2015, 2016, 2017, 2018, 2019, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with AWA.Services.Contexts;
with AWA.Workspaces.Models;
with AWA.Workspaces.Modules;
with AWA.Modules.Get;
with AWA.Permissions;
with AWA.Users.Models;
with AWA.Wikis.Beans;
with AWA.Modules.Beans;
with AWA.Storages.Models;
with AWA.Storages.Services;
with AWA.Storages.Modules;

with Ada.Strings;
with ADO.Objects;
with ADO.SQL;
with ADO.Queries;
with ADO.Statements;

with Util.Log.Loggers;
with Util.Strings.Tokenizers;
package body AWA.Wikis.Modules is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Awa.Wikis.Module");

   package Register is new AWA.Modules.Beans (Module => Wiki_Module,
                                              Module_Access => Wiki_Module_Access);

   --  ------------------------------
   --  Initialize the wikis module.
   --  ------------------------------
   overriding
   procedure Initialize (Plugin : in out Wiki_Module;
                         App    : in AWA.Modules.Application_Access;
                         Props  : in ASF.Applications.Config) is
   begin
      Log.Info ("Initializing the wikis module");

      --  Setup the resource bundles.
      App.Register ("wikiMsg", "wikis");

      --  Register here any bean class, servlet, filter.
      Register.Register (Plugin => Plugin,
                         Name   => "AWA.Wikis.Beans.Wiki_Space_Bean",
                         Handler => AWA.Wikis.Beans.Create_Wiki_Space_Bean'Access);

      Register.Register (Plugin => Plugin,
                         Name   => "AWA.Wikis.Beans.Wiki_Admin_Bean",
                         Handler => AWA.Wikis.Beans.Create_Wiki_Admin_Bean'Access);

      Register.Register (Plugin => Plugin,
                         Name   => "AWA.Wikis.Beans.Wiki_Page_Bean",
                         Handler => AWA.Wikis.Beans.Create_Wiki_Page_Bean'Access);

      Register.Register (Plugin => Plugin,
                         Name   => "AWA.Wikis.Beans.Wiki_Image_Info_Bean",
                         Handler => AWA.Wikis.Beans.Create_Wiki_Image_Info_Bean'Access);

      Register.Register (Plugin => Plugin,
                         Name   => "AWA.Wikis.Beans.Wiki_Page_Info_Bean",
                         Handler => AWA.Wikis.Beans.Create_Wiki_Page_Info_Bean'Access);

      Register.Register (Plugin => Plugin,
                         Name   => "AWA.Wikis.Beans.Wiki_List_Bean",
                         Handler => AWA.Wikis.Beans.Create_Wiki_List_Bean'Access);

      Register.Register (Plugin => Plugin,
                         Name   => "AWA.Wikis.Beans.Format_List_Bean",
                         Handler => AWA.Wikis.Beans.Create_Format_List_Bean'Access);

      Register.Register (Plugin => Plugin,
                         Name   => "AWA.Wikis.Beans.Wiki_View_Bean",
                         Handler => AWA.Wikis.Beans.Create_Wiki_View_Bean'Access);

      Register.Register (Plugin => Plugin,
                         Name   => "AWA.Wikis.Beans.Wiki_Version_List_Bean",
                         Handler => AWA.Wikis.Beans.Create_Wiki_Version_List_Bean'Access);

      App.Add_Servlet ("wiki-image", Plugin.Image_Servlet'Unchecked_Access);

      AWA.Modules.Module (Plugin).Initialize (App, Props);

      --  Add here the creation of manager instances.
   end Initialize;

   --  ------------------------------
   --  Configures the module after its initialization and after having read its XML configuration.
   --  ------------------------------
   overriding
   procedure Configure (Plugin : in out Wiki_Module;
                        Props  : in ASF.Applications.Config) is
      pragma Unreferenced (Props);

      Image_Prefix : constant String := Plugin.Get_Config (PARAM_IMAGE_PREFIX);
      Page_Prefix  : constant String := Plugin.Get_Config (PARAM_PAGE_PREFIX);
   begin
      Plugin.Image_Prefix := Wiki.Strings.To_UString (Wiki.Strings.To_WString (Image_Prefix));
      Plugin.Page_Prefix  := Wiki.Strings.To_UString (Wiki.Strings.To_WString (Page_Prefix));
   end Configure;

   --  ------------------------------
   --  Get the image prefix that was configured for the Wiki module.
   --  ------------------------------
   function Get_Image_Prefix (Module : in Wiki_Module)
                              return Wiki.Strings.UString is
   begin
      return Module.Image_Prefix;
   end Get_Image_Prefix;

   --  ------------------------------
   --  Get the page prefix that was configured for the Wiki module.
   --  ------------------------------
   function Get_Page_Prefix (Module : in Wiki_Module)
                             return Wiki.Strings.UString is
   begin
      return Module.Page_Prefix;
   end Get_Page_Prefix;

   --  ------------------------------
   --  Get the wikis module.
   --  ------------------------------
   function Get_Wiki_Module return Wiki_Module_Access is
      function Get is new AWA.Modules.Get (Wiki_Module, Wiki_Module_Access, NAME);
   begin
      return Get;
   end Get_Wiki_Module;

   --  ------------------------------
   --  Create the wiki space.
   --  ------------------------------
   procedure Create_Wiki_Space (Module : in Wiki_Module;
                                Wiki   : in out AWA.Wikis.Models.Wiki_Space_Ref'Class) is

      procedure Copy_Page (Item : in String;
                           Done : out Boolean);

      Ctx   : constant Services.Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      User  : constant ADO.Identifier := Ctx.Get_User_Identifier;
      DB    : ADO.Sessions.Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
      WS    : AWA.Workspaces.Models.Workspace_Ref;

      procedure Copy_Page (Item : in String;
                           Done : out Boolean) is
      begin
         Module.Copy_Page (DB, Wiki, ADO.Identifier'Value (Item));
         Done := False;

      exception
         when Constraint_Error =>
            Log.Error ("Invalid configuration wiki page id {0}", Item);

      end Copy_Page;

   begin
      Log.Info ("Creating new wiki space {0}", String '(Wiki.Get_Name));

      Ctx.Start;
      AWA.Workspaces.Modules.Get_Workspace (DB, Ctx, WS);

      --  Check that the user has the create permission on the given workspace.
      AWA.Permissions.Check (Permission => ACL_Create_Wiki_Space.Permission,
                             Entity     => WS);
      Wiki.Set_Workspace (WS);
      Wiki.Set_Create_Date (Ada.Calendar.Clock);
      Wiki.Save (DB);

      --  Add the permission for the user to use the new wiki space.
      AWA.Workspaces.Modules.Add_Permission (Session   => DB,
                                             User      => User,
                                             Entity    => Wiki,
                                             Workspace => WS.Get_Id,
                                             List      => (ACL_Update_Wiki_Space.Permission,
                                                           ACL_Delete_Wiki_Space.Permission,
                                                           ACL_Create_Wiki_Pages.Permission,
                                                           ACL_Delete_Wiki_Pages.Permission,
                                                           ACL_Update_Wiki_Pages.Permission,
                                                           ACL_View_Wiki_Page.Permission));

      Util.Strings.Tokenizers.Iterate_Tokens (Content => Module.Get_Config (PARAM_WIKI_COPY_LIST),
                                              Pattern => ",",
                                              Process => Copy_Page'Access,
                                              Going   => Ada.Strings.Forward);

      Ctx.Commit;

      Log.Info ("Wiki {0} created for user {1}",
                ADO.Identifier'Image (Wiki.Get_Id), ADO.Identifier'Image (User));
   end Create_Wiki_Space;

   --  ------------------------------
   --  Save the wiki space.
   --  ------------------------------
   procedure Save_Wiki_Space (Module : in Wiki_Module;
                              Wiki   : in out AWA.Wikis.Models.Wiki_Space_Ref'Class) is
      pragma Unreferenced (Module);

      Ctx   : constant Services.Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      DB    : ADO.Sessions.Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
   begin
      Log.Info ("Updating wiki space {0}", String '(Wiki.Get_Name));

      Ctx.Start;

      --  Check that the user has the update permission on the given wiki space.
      AWA.Permissions.Check (Permission => ACL_Update_Wiki_Space.Permission,
                             Entity     => Wiki);
      Wiki.Save (DB);
      Ctx.Commit;
   end Save_Wiki_Space;

   --  ------------------------------
   --  Load the wiki space.
   --  ------------------------------
   procedure Load_Wiki_Space (Module : in Wiki_Module;
                              Wiki   : in out AWA.Wikis.Models.Wiki_Space_Ref'Class;
                              Id     : in ADO.Identifier) is
      pragma Unreferenced (Module);

      Ctx   : constant Services.Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      DB    : ADO.Sessions.Session := AWA.Services.Contexts.Get_Session (Ctx);
      Found : Boolean;
   begin
      Wiki.Load (DB, Id, Found);
   end Load_Wiki_Space;

   --  ------------------------------
   --  Create the wiki page into the wiki space.
   --  ------------------------------
   procedure Create_Wiki_Page (Model   : in Wiki_Module;
                               Into    : in AWA.Wikis.Models.Wiki_Space_Ref'Class;
                               Page    : in out AWA.Wikis.Models.Wiki_Page_Ref'Class;
                               Content : in out AWA.Wikis.Models.Wiki_Content_Ref'Class) is
   begin
      Log.Info ("Create wiki page {0}", String '(Page.Get_Name));

      --  Check that the user has the create wiki page permission on the given wiki space.
      AWA.Permissions.Check (Permission => ACL_Create_Wiki_Pages.Permission,
                             Entity     => Into);

      Page.Set_Wiki (Into);
      Model.Save_Wiki_Content (Page, Content);
   end Create_Wiki_Page;

   --  ------------------------------
   --  Save the wiki page.
   --  ------------------------------
   procedure Save (Model  : in Wiki_Module;
                   Page   : in out AWA.Wikis.Models.Wiki_Page_Ref'Class) is
      pragma Unreferenced (Model);

      Ctx   : constant Services.Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      DB    : ADO.Sessions.Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
   begin
      --  Check that the user has the update wiki page permission on the given wiki space.
      AWA.Permissions.Check (Permission => ACL_Update_Wiki_Pages.Permission,
                             Entity     => Page);

      Ctx.Start;
      Page.Save (DB);
      Ctx.Commit;
   end Save;

   --  ------------------------------
   --  Delete the wiki page as well as all its versions.
   --  ------------------------------
   procedure Delete (Model  : in Wiki_Module;
                     Page   : in out AWA.Wikis.Models.Wiki_Page_Ref'Class) is
      Ctx   : constant Services.Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      DB    : ADO.Sessions.Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
   begin
      --  Check that the user has the delete wiki page permission on the given wiki page.
      AWA.Permissions.Check (Permission => ACL_Delete_Wiki_Pages.Permission,
                             Entity     => Page);

      Ctx.Start;

      --  Before deleting the wiki page, delete the version content.
      declare
         Stmt : ADO.Statements.Delete_Statement
           := DB.Create_Statement (AWA.Wikis.Models.WIKI_CONTENT_TABLE);
      begin
         Stmt.Set_Filter (Filter => "page_id = ?");
         Stmt.Add_Param (Value => Page);
         Stmt.Execute;
      end;

      --  Notify the deletion of the wiki page (before the real delete).
      Wiki_Lifecycle.Notify_Delete (Model, Page);
      Page.Delete (DB);
      Ctx.Commit;
   end Delete;

   --  ------------------------------
   --  Load the wiki page and its content.
   --  ------------------------------
   procedure Load_Page (Model   : in Wiki_Module;
                        Page    : in out AWA.Wikis.Models.Wiki_Page_Ref'Class;
                        Content : in out AWA.Wikis.Models.Wiki_Content_Ref'Class;
                        Tags    : in out AWA.Tags.Beans.Tag_List_Bean;
                        Id      : in ADO.Identifier) is
      DB    : ADO.Sessions.Session := Model.Get_Session;
      Found : Boolean;
   begin
      --  Check that the user has the view page permission on the given wiki page.
      AWA.Permissions.Check (Permission => ACL_View_Wiki_Page.Permission,
                             Entity     => Id);

      Page.Load (DB, Id, Found);
      Tags.Load_Tags (DB, Id);
      Content := Page.Get_Content;
      if not Content.Is_Null then
         Content.Load (DB, Content.Get_Id, Found);
      end if;
   end Load_Page;

   --  ------------------------------
   --  Load the wiki page and its content from the wiki space Id and the page name.
   --  ------------------------------
   procedure Load_Page (Model   : in Wiki_Module;
                        Page    : in out AWA.Wikis.Models.Wiki_Page_Ref'Class;
                        Content : in out AWA.Wikis.Models.Wiki_Content_Ref'Class;
                        Tags    : in out AWA.Tags.Beans.Tag_List_Bean;
                        Wiki    : in ADO.Identifier;
                        Name    : in String) is
      DB    : ADO.Sessions.Session := Model.Get_Session;
      Found : Boolean;
      Query : ADO.SQL.Query;
   begin
      Query.Bind_Param (1, Wiki);
      Query.Bind_Param (2, Name);
      Query.Set_Filter ("o.wiki_id = ? AND o.name = ?");
      Page.Find (DB, Query, Found);
      if not Found then
         raise ADO.Objects.NOT_FOUND;
      end if;

      --  Check that the user has the view page permission on the given wiki page.
      AWA.Permissions.Check (Permission => ACL_View_Wiki_Page.Permission,
                             Entity     => Page.Get_Id);

      Tags.Load_Tags (DB, Page.Get_Id);
      Content := Page.Get_Content;
      if not Content.Is_Null then
         Content.Load (DB, Content.Get_Id, Found);
      end if;
   end Load_Page;

   --  ------------------------------
   --  Create a new wiki content for the wiki page.
   --  ------------------------------
   procedure Create_Wiki_Content (Model   : in Wiki_Module;
                                  Page    : in out AWA.Wikis.Models.Wiki_Page_Ref'Class;
                                  Content : in out AWA.Wikis.Models.Wiki_Content_Ref'Class) is
   begin
      --  Check that the user has the update wiki content permission on the given wiki page.
      AWA.Permissions.Check (Permission => ACL_Update_Wiki_Pages.Permission,
                             Entity     => Page);

      Model.Save_Wiki_Content (Page, Content);
   end Create_Wiki_Content;

   --  ------------------------------
   --  Copy the wiki page with its last version to the wiki space.
   --  ------------------------------
   procedure Copy_Page (Module  : in Wiki_Module;
                        DB      : in out ADO.Sessions.Master_Session;
                        Wiki    : in AWA.Wikis.Models.Wiki_Space_Ref'Class;
                        Page_Id : in ADO.Identifier) is
      Page    : AWA.Wikis.Models.Wiki_Page_Ref;
      Found   : Boolean;
   begin
      Page.Load (DB, Page_Id, Found);
      if not Found then
         Log.Warn ("Wiki page copy is abandoned: page {0} was not found",
                   ADO.Identifier'Image (Page_Id));
         return;
      end if;
      Module.Copy_Page (DB, Wiki, Page);
   end Copy_Page;

   --  ------------------------------
   --  Copy the wiki page with its last version to the wiki space.
   --  ------------------------------
   procedure Copy_Page (Module  : in Wiki_Module;
                        DB      : in out ADO.Sessions.Master_Session;
                        Wiki    : in AWA.Wikis.Models.Wiki_Space_Ref'Class;
                        Page    : in AWA.Wikis.Models.Wiki_Page_Ref'Class) is
      New_Content : AWA.Wikis.Models.Wiki_Content_Ref;
      New_Page    : AWA.Wikis.Models.Wiki_Page_Ref;
   begin
      New_Page.Set_Wiki (Wiki);
      New_Page.Set_Title (String '(Page.Get_Title));
      New_Page.Set_Name (String '(Page.Get_Name));
      New_Page.Set_Is_Public (Wiki.Get_Is_Public);
      New_Page.Set_Last_Version (0);
      New_Content.Set_Content (String '(Page.Get_Content.Get_Content));
      Module.Save_Wiki_Content (DB, New_Page, New_Content);
   end Copy_Page;

   --  ------------------------------
   --  Save a new wiki content for the wiki page.
   --  ------------------------------
   procedure Save_Wiki_Content (Model   : in Wiki_Module;
                                Page    : in out AWA.Wikis.Models.Wiki_Page_Ref'Class;
                                Content : in out AWA.Wikis.Models.Wiki_Content_Ref'Class) is
      Ctx : constant Services.Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      DB  : ADO.Sessions.Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
   begin
      Ctx.Start;
      Model.Save_Wiki_Content (DB, Page, Content);
      Ctx.Commit;
   end Save_Wiki_Content;

   --  ------------------------------
   --  Save a new wiki content for the wiki page.
   --  ------------------------------
   procedure Save_Wiki_Content (Model   : in Wiki_Module;
                                DB      : in out ADO.Sessions.Master_Session;
                                Page    : in out AWA.Wikis.Models.Wiki_Page_Ref'Class;
                                Content : in out AWA.Wikis.Models.Wiki_Content_Ref'Class) is
      Ctx     : constant Services.Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      User    : constant AWA.Users.Models.User_Ref := Ctx.Get_User;
      Created : constant Boolean := not Page.Is_Inserted;
      Query   : ADO.Queries.Context;
      Stmt    : ADO.Statements.Query_Statement;
   begin
      --  Check if the wiki page name is already used.
      Query.Set_Query (AWA.Wikis.Models.Query_Wiki_Page_Name_Count);
      Query.Bind_Param ("name", String '(Page.Get_Name));
      Query.Bind_Param ("id", Page.Get_Id);
      Query.Bind_Param ("wiki_id", Page.Get_Wiki.Get_Id);
      Stmt := DB.Create_Statement (Query);
      Stmt.Execute;
      if Stmt.Get_Result_Integer /= 0 then
         raise Name_Used;
      end if;

      Page.Set_Last_Version (Page.Get_Last_Version + 1);
      if Created then
         Page.Save (DB);
      end if;
      Content.Set_Page_Id (ADO.Objects.Get_Value (Page.Get_Key));
      Content.Set_Create_Date (Ada.Calendar.Clock);
      Content.Set_Author (User);
      Content.Set_Page_Version (Page.Get_Last_Version);
      Content.Save (DB);
      Page.Set_Content (Content);
      Page.Save (DB);

      if Created then
         Wiki_Lifecycle.Notify_Create (Model, Page);
      else
         Wiki_Lifecycle.Notify_Update (Model, Page);
      end if;
   end Save_Wiki_Content;

   procedure Load_Image (Model    : in Wiki_Module;
                         Wiki_Id  : in ADO.Identifier;
                         Image_Id : in ADO.Identifier;
                         Width    : in out Natural;
                         Height   : in out Natural;
                         Mime     : out Ada.Strings.Unbounded.Unbounded_String;
                         Date     : out Ada.Calendar.Time;
                         Into     : out ADO.Blob_Ref) is
      pragma Unreferenced (Model);
      use type AWA.Storages.Models.Storage_Type;

      Ctx   : constant Services.Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      User  : constant ADO.Identifier := Ctx.Get_User_Identifier;
      DB    : constant ADO.Sessions.Session := AWA.Services.Contexts.Get_Session (Ctx);
      Query : ADO.Statements.Query_Statement;
      Kind  : AWA.Storages.Models.Storage_Type;
   begin
      if Width = Natural'Last or else Height = Natural'Last then
         Query := DB.Create_Statement (Models.Query_Wiki_Image_Get_Data);
      elsif Width > 0 then
         Query := DB.Create_Statement (Models.Query_Wiki_Image_Width_Get_Data);
         Query.Bind_Param ("width", Width);
      elsif Height > 0 then
         Query := DB.Create_Statement (Models.Query_Wiki_Image_Height_Get_Data);
         Query.Bind_Param ("height", Height);
      else
         Query := DB.Create_Statement (Models.Query_Wiki_Image_Get_Data);
      end if;
      Query.Bind_Param ("wiki_id", Wiki_Id);
      Query.Bind_Param ("store_id", Image_Id);
      Query.Bind_Param ("user_id", User);

      Query.Execute;
      if not Query.Has_Elements then
         Log.Warn ("Wiki image entity {0} not found", ADO.Identifier'Image (Image_Id));
         raise ADO.Objects.NOT_FOUND;
      end if;
      Mime   := Query.Get_Unbounded_String (1);
      Date   := Query.Get_Time (2);
      Width  := Query.Get_Natural (5);
      Height := Query.Get_Natural (6);
      Kind   := AWA.Storages.Models.Storage_Type'Val (Query.Get_Integer (4));
      if Kind = AWA.Storages.Models.DATABASE then
         Into := Query.Get_Blob (7);
      else
         declare
            Storage : constant AWA.Storages.Services.Storage_Service_Access
              := AWA.Storages.Modules.Get_Storage_Manager;
         begin
            Storage.Load (Query.Get_Identifier (0), Kind, Into);
         end;
      end if;
   end Load_Image;

end AWA.Wikis.Modules;
