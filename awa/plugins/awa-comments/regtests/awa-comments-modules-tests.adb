-----------------------------------------------------------------------
--  awa-comments-modules-tests -- Unit tests for comments module
--  Copyright (C) 2014, 2019 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Strings.Unbounded;

with Util.Test_Caller;
with Util.Beans.Basic;

with Security.Contexts;

with ADO.Utils;

with AWA.Users.Models;
with AWA.Services.Contexts;
with AWA.Tests.Helpers.Users;
with AWA.Comments.Beans;
package body AWA.Comments.Modules.Tests is

   use ADO;

   package Caller is new Util.Test_Caller (Test, "Comments.Modules");

   function Get_Count (Bean : in Util.Beans.Objects.Object) return Natural;

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test AWA.Comments.Modules.Create_Comment (PUBLISHED)",
                       Test_Create_Published_Comment'Access);
      Caller.Add_Test (Suite, "Test AWA.Comments.Modules.Create_Comment (WAITING)",
                       Test_Create_Waiting_Comment'Access);
      Caller.Add_Test (Suite, "Test AWA.Comments.Modules.Delete_Comment",
                       Test_Remove_Comment'Access);
      Caller.Add_Test (Suite, "Test AWA.Comments.Modules.Update_Comment (publish)",
                       Test_Update_Comment'Access);
      Caller.Add_Test (Suite, "Test AWA.Comments.Modules.Publish_Comment (publish)",
                       Test_Publish_Comment'Access);
   end Add_Tests;

   function Get_Count (Bean : in Util.Beans.Objects.Object) return Natural is
      Value : constant Util.Beans.Objects.Object := Util.Beans.Objects.Get_Value (Bean, "count");
   begin
      return Util.Beans.Objects.To_Integer (Value);
   end Get_Count;

   procedure List_Comments (T    : in out Test;
                            User : in ADO.Identifier;
                            Into : out Util.Beans.Objects.Object) is
      Comment_Manager  : constant Comment_Module_Access := Get_Comment_Module;
      Bean             : Util.Beans.Basic.Readonly_Bean_Access;
      List             : AWA.Comments.Beans.Comment_List_Bean_Access;
   begin
      Bean := Beans.Create_Comment_List_Bean (Comment_Manager);
      Into := Util.Beans.Objects.To_Object (Bean.all'Access);
      List := AWA.Comments.Beans.Comment_List_Bean'Class (Bean.all)'Access;
      List.Set_Value ("entity_type", Util.Beans.Objects.To_Object (String '("awa_user")));
      Util.Tests.Assert_Equals (T, 0, Integer (List.Get_Count), "Invalid number of comments");

      --  Load the existing comments.
      List.Load_Comments (User);
   end List_Comments;

   --  ------------------------------
   --  Create a comment and return the list of comments before and after the creation.
   --  ------------------------------
   procedure Create_Comment (T : in out Test;
                             Status : in AWA.Comments.Models.Status_Type;
                             Before : out Util.Beans.Objects.Object;
                             After  : out Util.Beans.Objects.Object;
                             Id     : out ADO.Identifier) is
      Sec_Ctx      : Security.Contexts.Security_Context;
      Context      : AWA.Services.Contexts.Service_Context;
   begin
      AWA.Tests.Helpers.Users.Login (Context, Sec_Ctx, "test-add-comment@test.com");

      declare
         Comment_Manager  : constant Comment_Module_Access := Get_Comment_Module;
         User             : constant AWA.Users.Models.User_Ref := Context.Get_User;
         Bean             : Util.Beans.Basic.Readonly_Bean_Access;
         Comment          : AWA.Comments.Beans.Comment_Bean_Access;
         Cleanup          : Util.Beans.Objects.Object;
         Outcome          : Ada.Strings.Unbounded.Unbounded_String;
      begin
         T.Assert (Comment_Manager /= null, "There is no comment module");

         T.List_Comments (User.Get_Id, Before);

         --  Create a new comment associated with the current user.
         Bean := Beans.Create_Comment_Bean (Comment_Manager);
         Cleanup := Util.Beans.Objects.To_Object (Bean.all'Access);
         Comment := AWA.Comments.Beans.Comment_Bean'Class (Bean.all)'Access;
         Comment.Set_Value ("entity_type", Util.Beans.Objects.To_Object (String '("awa_user")));
         Comment.Set_Value ("permission", Util.Beans.Objects.To_Object (String '("logged-user")));
         Comment.Set_Status (Status);
         Comment.Set_Entity_Id (User.Get_Id);

         --  Create the comment.
         Comment.Set_Message ("the comment message for the current user " &
                              AWA.Comments.Models.Status_Type'Image (Status));
         Comment.Create (Outcome);
         Id := Comment.Get_Id;
         T.Assert (Id /= ADO.NO_IDENTIFIER, "Invalid new comment identifier");

         --  Load again the comments.
         T.List_Comments (User.Get_Id, After);
         T.Assert (not Util.Beans.Objects.Is_Null (Cleanup), "Comment bean is null");
      end;
      T.Assert (not Util.Beans.Objects.Is_Null (Before), "Before list instance is null");
      T.Assert (not Util.Beans.Objects.Is_Null (After), "After list instance is null");
   end Create_Comment;

   --  ------------------------------
   --  Test comment creation (PUBLISHED).
   --  ------------------------------
   procedure Test_Create_Published_Comment (T : in out Test) is
      Before   : Util.Beans.Objects.Object;
      After    : Util.Beans.Objects.Object;
      Id       : ADO.Identifier;
   begin
      T.Create_Comment (AWA.Comments.Models.COMMENT_PUBLISHED, Before, After, Id);
      declare
         Before_Count : constant Natural := Get_Count (Before);
         After_Count  : constant Natural := Get_Count (After);
      begin
         Util.Tests.Assert_Equals (T, Before_Count + 1, After_Count,
                                   "The new comment does not appear in the list");
      end;
   end Test_Create_Published_Comment;

   --  ------------------------------
   --  Test comment creation (WAITING).
   --  ------------------------------
   procedure Test_Create_Waiting_Comment (T : in out Test) is
      Before   : Util.Beans.Objects.Object;
      After    : Util.Beans.Objects.Object;
      Id       : ADO.Identifier;
   begin
      T.Create_Comment (AWA.Comments.Models.COMMENT_WAITING, Before, After, Id);
      declare
         Before_Count : constant Natural := Get_Count (Before);
         After_Count  : constant Natural := Get_Count (After);
      begin
         Util.Tests.Assert_Equals (T, Before_Count, After_Count,
                                   "The new comment MUST not appear in the list");
      end;
   end Test_Create_Waiting_Comment;

   --  ------------------------------
   --  Test comment removal.
   --  ------------------------------
   procedure Test_Remove_Comment (T : in out Test) is
      Sec_Ctx  : Security.Contexts.Security_Context;
      Context  : AWA.Services.Contexts.Service_Context;
      Before   : Util.Beans.Objects.Object;
      After    : Util.Beans.Objects.Object;
      Id       : ADO.Identifier;
   begin
      T.Create_Comment (AWA.Comments.Models.COMMENT_PUBLISHED, Before, After, Id);
      Util.Tests.Assert_Equals (T, Get_Count (Before) + 1, Get_Count (After),
                                "The new comment MUST have been added in the list");

      --  Now, simulate a user that logs in and deletes the comment.
      AWA.Tests.Helpers.Users.Login (Context, Sec_Ctx, "test-add-comment@test.com");
      declare
         Comment_Manager  : constant Comment_Module_Access := Get_Comment_Module;
         User             : constant AWA.Users.Models.User_Ref := Context.Get_User;
         Comment          : AWA.Comments.Beans.Comment_Bean_Access;
         Bean             : Util.Beans.Basic.Readonly_Bean_Access;
         Outcome          : Ada.Strings.Unbounded.Unbounded_String;
         Cleanup          : Util.Beans.Objects.Object;
      begin
         T.Assert (Comment_Manager /= null, "There is no comment module");

         --  Create the comment bean for the deletion.
         Bean := Beans.Create_Comment_Bean (Comment_Manager);
         Cleanup := Util.Beans.Objects.To_Object (Bean.all'Access);
         Comment := AWA.Comments.Beans.Comment_Bean'Class (Bean.all)'Access;
         Comment.Set_Value ("entity_type", Util.Beans.Objects.To_Object (String '("awa_user")));
         Comment.Set_Value ("permission", Util.Beans.Objects.To_Object (String '("logged-user")));
         Comment.Set_Value ("id", ADO.Utils.To_Object (Id));

         Comment.Delete (Outcome);

         T.List_Comments (User.Get_Id, After);
         T.Assert (not Util.Beans.Objects.Is_Null (Cleanup), "Comment bean is null");
      end;
      Util.Tests.Assert_Equals (T, Get_Count (Before), Get_Count (After),
                                "The new comment MUST have been removed from the list");
   end Test_Remove_Comment;

   --  ------------------------------
   --  Test comment publication.
   --  ------------------------------
   procedure Test_Update_Comment (T : in out Test) is
      Before    : Util.Beans.Objects.Object;
      After     : Util.Beans.Objects.Object;
      Sec_Ctx   : Security.Contexts.Security_Context;
      Context   : AWA.Services.Contexts.Service_Context;
      Id        : ADO.Identifier;
   begin
      T.Create_Comment (AWA.Comments.Models.COMMENT_WAITING, Before, After, Id);
      Util.Tests.Assert_Equals (T, Get_Count (Before), Get_Count (After),
                                "The new comment MUST not be in the list");

      --  Now, simulate a user that logs in and publishes the comment.
      AWA.Tests.Helpers.Users.Login (Context, Sec_Ctx, "test-add-comment@test.com");
      declare
         Comment_Manager  : constant Comment_Module_Access := Get_Comment_Module;
         User             : constant AWA.Users.Models.User_Ref := Context.Get_User;
         Comment          : AWA.Comments.Beans.Comment_Bean_Access;
         Bean             : Util.Beans.Basic.Readonly_Bean_Access;
         Outcome          : Ada.Strings.Unbounded.Unbounded_String;
         Cleanup          : Util.Beans.Objects.Object;
      begin
         T.Assert (Comment_Manager /= null, "There is no comment module");

         --  Create the comment bean for the deletion.
         Bean := Beans.Create_Comment_Bean (Comment_Manager);
         Cleanup := Util.Beans.Objects.To_Object (Bean.all'Access);
         Comment := AWA.Comments.Beans.Comment_Bean'Class (Bean.all)'Access;
         Comment.Set_Value ("entity_type", Util.Beans.Objects.To_Object (String '("awa_user")));
         Comment.Set_Value ("permission", Util.Beans.Objects.To_Object (String '("logged-user")));
         Comment.Set_Value ("id", ADO.Utils.To_Object (Id));

         Comment.Set_Status (AWA.Comments.Models.COMMENT_PUBLISHED);
         Comment.Save (Outcome);

         T.List_Comments (User.Get_Id, After);
         T.Assert (not Util.Beans.Objects.Is_Null (Cleanup), "Comment bean is null");
      end;
      Util.Tests.Assert_Equals (T, Get_Count (Before) + 1, Get_Count (After),
                                "The new comment MUST be present in the list after publication");
   end Test_Update_Comment;

   --  ------------------------------
   --  Test comment publication through the publish operation bean.
   --  ------------------------------
   procedure Test_Publish_Comment (T : in out Test) is
      Before    : Util.Beans.Objects.Object;
      After     : Util.Beans.Objects.Object;
      Sec_Ctx   : Security.Contexts.Security_Context;
      Context   : AWA.Services.Contexts.Service_Context;
      Id        : ADO.Identifier;
   begin
      T.Create_Comment (AWA.Comments.Models.COMMENT_WAITING, Before, After, Id);
      Util.Tests.Assert_Equals (T, Get_Count (Before), Get_Count (After),
                                "The new comment MUST not be in the list");

      --  Now, simulate a user that logs in and publishes the comment.
      AWA.Tests.Helpers.Users.Login (Context, Sec_Ctx, "test-add-comment@test.com");
      declare
         Comment_Manager  : constant Comment_Module_Access := Get_Comment_Module;
         User             : constant AWA.Users.Models.User_Ref := Context.Get_User;
         Comment          : AWA.Comments.Beans.Comment_Bean;
      begin
         T.Assert (Comment_Manager /= null, "There is no comment module");

         Comment_Manager.Publish_Comment ("logged-user", Id, AWA.Comments.Models.COMMENT_PUBLISHED,
                                          Comment);
         T.Assert (not Comment.Is_Null, "Comment bean is null");

         T.List_Comments (User.Get_Id, After);
      end;
      Util.Tests.Assert_Equals (T, Get_Count (Before) + 1, Get_Count (After),
                                "The new comment MUST be present in the list after publication");
   end Test_Publish_Comment;

end AWA.Comments.Modules.Tests;
