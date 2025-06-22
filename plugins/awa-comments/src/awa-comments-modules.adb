-----------------------------------------------------------------------
--  awa-comments-module -- Comments module
--  Copyright (C) 2011, 2012, 2013, 2014 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Calendar;

with Util.Log.Loggers;

with ADO.Sessions;
with ADO.Sessions.Entities;
with Security.Permissions;

with AWA.Users.Models;
with AWA.Permissions;
with AWA.Modules.Beans;
with AWA.Services.Contexts;
with AWA.Comments.Beans;

package body AWA.Comments.Modules is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("AWA.Comments.Module");

   package Register is new AWA.Modules.Beans (Module        => Comment_Module,
                                              Module_Access => Comment_Module_Access);

   --  ------------------------------
   --  Initialize the comments module.
   --  ------------------------------
   overriding
   procedure Initialize (Plugin : in out Comment_Module;
                         App    : in AWA.Modules.Application_Access;
                         Props  : in ASF.Applications.Config) is
   begin
      Log.Info ("Initializing the comments module");

      --  Setup the resource bundles.
      App.Register ("commentMsg", "comments");

      --  Register the comment list bean.
      Register.Register (Plugin => Plugin,
                         Name   => "AWA.Comments.Beans.Comment_List_Bean",
                         Handler => AWA.Comments.Beans.Create_Comment_List_Bean'Access);

      --  Register the comment bean.
      Register.Register (Plugin => Plugin,
                         Name   => "AWA.Comments.Beans.Comment_Bean",
                         Handler => AWA.Comments.Beans.Create_Comment_Bean'Access);

      AWA.Modules.Module (Plugin).Initialize (App, Props);
   end Initialize;

   --  ------------------------------
   --  Load the comment identified by the given identifier.
   --  ------------------------------
   procedure Load_Comment (Model   : in Comment_Module;
                           Comment : in out AWA.Comments.Models.Comment_Ref'Class;
                           Id      : in ADO.Identifier) is
      pragma Unreferenced (Model);

      Ctx   : constant Services.Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      DB    : ADO.Sessions.Session := AWA.Services.Contexts.Get_Session (Ctx);
      Found : Boolean;
   begin
      Comment.Load (DB, Id, Found);
   end Load_Comment;

   --  ------------------------------
   --  Create a new comment for the associated database entity.
   --  ------------------------------
   procedure Create_Comment (Model       : in Comment_Module;
                             Permission  : in String;
                             Entity_Type : in String;
                             Comment     : in out AWA.Comments.Models.Comment_Ref'Class) is
      Ctx   : constant Services.Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      User  : constant AWA.Users.Models.User_Ref := Ctx.Get_User;
      DB    : ADO.Sessions.Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
   begin
      Ctx.Start;
      Log.Info ("Creating new comment for {0}", ADO.Identifier'Image (Comment.Get_Entity_Id));

      --  Check that the user has the create permission on the given workspace.
      AWA.Permissions.Check (Permission => Security.Permissions.Get_Permission_Index (Permission),
                             Entity     => Comment.Get_Entity_Id);

      Comment.Set_Entity_Type (ADO.Sessions.Entities.Find_Entity_Type (DB, Entity_Type));
      Comment.Set_Author (User);
      Comment.Set_Create_Date (Ada.Calendar.Clock);
      Comment.Save (DB);

      Comment_Lifecycle.Notify_Create (Model, Comment);
      Ctx.Commit;
   end Create_Comment;

   --  ------------------------------
   --  Update the comment represented by <tt>Comment</tt> if the current user has the
   --  permission identified by <tt>Permission</tt>.
   --  ------------------------------
   procedure Update_Comment (Model       : in Comment_Module;
                             Permission  : in String;
                             Comment     : in out AWA.Comments.Models.Comment_Ref'Class) is
      Ctx   : constant Services.Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      DB    : ADO.Sessions.Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
   begin
      if not Comment.Is_Inserted then
         Log.Error ("The comment was not created");
         raise Not_Found with "The comment was not inserted";
      end if;
      Ctx.Start;
      Log.Info ("Updating the comment {0}", ADO.Identifier'Image (Comment.Get_Id));

      --  Check that the user has the update permission on the given comment.
      AWA.Permissions.Check (Permission => Security.Permissions.Get_Permission_Index (Permission),
                             Entity     => Comment);
      Comment_Lifecycle.Notify_Update (Model, Comment);
      Comment.Save (DB);
      Ctx.Commit;
   end Update_Comment;

   --  ------------------------------
   --  Delete the comment represented by <tt>Comment</tt> if the current user has the
   --  permission identified by <tt>Permission</tt>.
   --  ------------------------------
   procedure Delete_Comment (Model       : in Comment_Module;
                             Permission  : in String;
                             Comment     : in out AWA.Comments.Models.Comment_Ref'Class) is
      Ctx   : constant Services.Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      DB    : ADO.Sessions.Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
   begin
      Ctx.Start;

      --  Check that the user has the delete permission on the given answer.
      AWA.Permissions.Check (Permission => Security.Permissions.Get_Permission_Index (Permission),
                             Entity     => Comment);

      Comment_Lifecycle.Notify_Delete (Model, Comment);
      Comment.Delete (DB);
      Ctx.Commit;
   end Delete_Comment;

   --  ------------------------------
   --  Set the publication status of the comment represented by <tt>Comment</tt>
   --  if the current user has the permission identified by <tt>Permission</tt>.
   --  ------------------------------
   procedure Publish_Comment (Model       : in Comment_Module;
                              Permission  : in String;
                              Id          : in ADO.Identifier;
                              Status      : in AWA.Comments.Models.Status_Type;
                              Comment     : in out AWA.Comments.Models.Comment_Ref'Class) is
      pragma Unreferenced (Model);

      Ctx   : constant Services.Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      DB    : ADO.Sessions.Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
   begin
      Ctx.Start;

      --  Check that the user has the permission to publish for the given comment.
      AWA.Permissions.Check (Permission => Security.Permissions.Get_Permission_Index (Permission),
                             Entity     => Id);

      Comment.Load (DB, Id);
      Comment.Set_Status (Status);
      Comment.Save (DB);
      Ctx.Commit;
   end Publish_Comment;

end AWA.Comments.Modules;
