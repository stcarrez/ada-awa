-----------------------------------------------------------------------
--  awa-comments-modules-tests -- Unit tests for comments module
--  Copyright (C) 2014 Stephane Carrez
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

with Util.Test_Caller;
with Util.Beans.Basic;
with Util.Beans.Objects;

with Security.Contexts;

with AWA.Users.Models;
with AWA.Services.Contexts;
with AWA.Tests.Helpers.Users;
with AWA.Comments.Beans;
package body AWA.Comments.Modules.Tests is

   use Util.Tests;
   use ADO;

   package Caller is new Util.Test_Caller (Test, "Comments.Modules");
--
--     function Create_Tag_List_Bean (Module : in Tag_Module_Access)
--                                    return AWA.Tags.Beans.Tag_List_Bean_Access;

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test AWA.Comments.Modules.Create_Comment (PUBLISHED)",
                       Test_Create_Published_Comment'Access);
      Caller.Add_Test (Suite, "Test AWA.Comments.Modules.Create_Comment (WAITING)",
                       Test_Create_Waiting_Comment'Access);
      Caller.Add_Test (Suite, "Test AWA.Tags.Modules.Remove_Tag",
                       Test_Remove_Comment'Access);
      Caller.Add_Test (Suite, "Test AWA.Tags.Modules.Update_Tags",
                       Test_Publish_Comment'Access);
   end Add_Tests;

   function Get_Count (Bean : in Util.Beans.Objects.Object) return Natural is
      Value : constant Util.Beans.Objects.Object := Util.Beans.Objects.Get_Value (Bean, "count");
   begin
      return Util.Beans.Objects.To_Integer (Value);
   end Get_Count;

   --  ------------------------------
   --  Create a comment and return the list of comments before and after the creation.
   --  ------------------------------
   procedure Create_Comment (T : in out Test;
                             Status : in AWA.Comments.Models.Status_Type;
                             Before : out Util.Beans.Objects.Object;
                             After  : out Util.Beans.Objects.Object) is
      Sec_Ctx      : Security.Contexts.Security_Context;
      Context      : AWA.Services.Contexts.Service_Context;
   begin
      AWA.Tests.Helpers.Users.Login (Context, Sec_Ctx, "test-add-comment@test.com");

      declare
         Comment_Manager  : constant Comment_Module_Access := Get_Comment_Module;
         User             : constant AWA.Users.Models.User_Ref := Context.Get_User;
         Bean             : Util.Beans.Basic.Readonly_Bean_Access;
         List             : AWA.Comments.Beans.Comment_List_Bean_Access;
         Comment          : AWA.Comments.Beans.Comment_Bean_Access;
         Cleanup          : Util.Beans.Objects.Object;
         Outcome          : Ada.Strings.Unbounded.Unbounded_String;
         Count            : Natural;
      begin
         T.Assert (Comment_Manager /= null, "There is no comment module");

         Bean := Beans.Create_Comment_List_Bean (Comment_Manager);
         Before := Util.Beans.Objects.To_Object (Bean.all'Access);
         List := AWA.Comments.Beans.Comment_List_Bean'Class (Bean.all)'Access;
         List.Set_Value ("entity_type", Util.Beans.Objects.To_Object (String '("awa_user")));
         Util.Tests.Assert_Equals (T, 0, Integer (List.Get_Count), "Invalid number of tags");

         --  Load the existing comments.
         List.Load_Comments (User.Get_Id);
         Count := List.Get_Count;

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

         --  Load again the comments.
         Bean := Beans.Create_Comment_List_Bean (Comment_Manager);
         After := Util.Beans.Objects.To_Object (Bean.all'Access);
         List := AWA.Comments.Beans.Comment_List_Bean'Class (Bean.all)'Access;
         List.Set_Value ("entity_type", Util.Beans.Objects.To_Object (String '("awa_user")));
         List.Load_Comments (User.Get_Id);
      end;
      T.Assert (not Util.Beans.Objects.Is_Null (Before), "Before list instance is null");
      T.Assert (not Util.Beans.Objects.Is_Null (After), "After list instance is null");
   end Create_Comment;

   --  ------------------------------
   --  Test comment creation (PUBLISHED).
   --  ------------------------------
   procedure Test_Create_Published_Comment (T : in out Test) is
      Before       : Util.Beans.Objects.Object;
      After        : Util.Beans.Objects.Object;
   begin
      T.Create_Comment (AWA.Comments.Models.COMMENT_PUBLISHED, Before, After);
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
      Before       : Util.Beans.Objects.Object;
      After        : Util.Beans.Objects.Object;
   begin
      T.Create_Comment (AWA.Comments.Models.COMMENT_WAITING, Before, After);
      declare
         Before_Count : constant Natural := Get_Count (Before);
         After_Count  : constant Natural := Get_Count (After);
      begin
         Util.Tests.Assert_Equals (T, Before_Count, After_Count,
                                   "The new comment MUST not appear in the list");
      end;
   end Test_Create_Waiting_Comment;

   --  ------------------------------
   --  Test tag removal.
   --  ------------------------------
   procedure Test_Remove_Comment (T : in out Test) is
      Sec_Ctx      : Security.Contexts.Security_Context;
      Context      : AWA.Services.Contexts.Service_Context;
   begin
      AWA.Tests.Helpers.Users.Login (Context, Sec_Ctx, "test-tag@test.com");
--
--        declare
--           Tag_Manager  : constant Tag_Module_Access := Get_Tag_Module;
--           User         : constant AWA.Users.Models.User_Ref := Context.Get_User;
--           List         : AWA.Tags.Beans.Tag_List_Bean_Access;
--           Cleanup      : Util.Beans.Objects.Object;
--        begin
--           T.Assert (Tag_Manager /= null, "There is no tag module");
--
--           List := Create_Tag_List_Bean (Tag_Manager);
--           Cleanup := Util.Beans.Objects.To_Object (List.all'Access);
--           List.Set_Value ("entity_type", Util.Beans.Objects.To_Object (String '("awa_user")));
--
--           Tag_Manager.Add_Tag (User.Get_Id, "awa_user", "workspaces-create", "user-tag-1");
--           Tag_Manager.Add_Tag (User.Get_Id, "awa_user", "workspaces-create", "user-tag-2");
--           Tag_Manager.Add_Tag (User.Get_Id, "awa_user", "workspaces-create", "user-tag-3");
--
--           Tag_Manager.Remove_Tag (User.Get_Id, "awa_user", "workspaces-create", "user-tag-2");
--           Tag_Manager.Remove_Tag (User.Get_Id, "awa_user", "workspaces-create", "user-tag-1");
--
--           --  Load the list.
--           List.Load_Tags (Tag_Manager.Get_Session, User.Get_Id);
--           Util.Tests.Assert_Equals (T, 1, Integer (List.Get_Count), "Invalid number of tags");
--           T.Assert (not Util.Beans.Objects.Is_Null (Cleanup), "Cleanup instance is null");
--        end;
   end Test_Remove_Comment;

   --  ------------------------------
   --  Test tag creation and removal.
   --  ------------------------------
   procedure Test_Publish_Comment (T : in out Test) is
      Sec_Ctx      : Security.Contexts.Security_Context;
      Context      : AWA.Services.Contexts.Service_Context;
   begin
      AWA.Tests.Helpers.Users.Login (Context, Sec_Ctx, "test-tag@test.com");

--        declare
--           Tag_Manager  : constant Tag_Module_Access := Get_Tag_Module;
--           User         : constant AWA.Users.Models.User_Ref := Context.Get_User;
--           List         : AWA.Tags.Beans.Tag_List_Bean_Access;
--           Cleanup      : Util.Beans.Objects.Object;
--           Tags         : Util.Strings.Vectors.Vector;
--        begin
--           T.Assert (Tag_Manager /= null, "There is no tag module");
--
--           List := Create_Tag_List_Bean (Tag_Manager);
--           Cleanup := Util.Beans.Objects.To_Object (List.all'Access);
--           List.Set_Value ("entity_type", Util.Beans.Objects.To_Object (String '("awa_user")));
--           List.Set_Value ("permission", Util.Beans.Objects.To_Object (String '("workspace-create")));
--
--           --  Add 3 tags.
--           Tags.Append ("user-tag-1");
--           Tags.Append ("user-tag-2");
--           Tags.Append ("user-tag-3");
--           List.Set_Added (Tags);
--
--           List.Update_Tags (User.Get_Id);
--
--           --  Load the list.
--           List.Load_Tags (Tag_Manager.Get_Session, User.Get_Id);
--           Util.Tests.Assert_Equals (T, 3, Integer (List.Get_Count), "Invalid number of tags");
--
--           --  Remove a tag that was not created.
--           Tags.Append ("user-tag-4");
--           List.Set_Deleted (Tags);
--
--           Tags.Clear;
--           Tags.Append ("user-tag-5");
--           List.Set_Added (Tags);
--           List.Update_Tags (User.Get_Id);
--
--           --  'user-tag-5' is the only tag that should exist now.
--           List.Load_Tags (Tag_Manager.Get_Session, User.Get_Id);
--           Util.Tests.Assert_Equals (T, 1, Integer (List.Get_Count), "Invalid number of tags");
--
--           T.Assert (not Util.Beans.Objects.Is_Null (Cleanup), "Cleanup instance is null");
--        end;
   end Test_Publish_Comment;

end AWA.Comments.Modules.Tests;
