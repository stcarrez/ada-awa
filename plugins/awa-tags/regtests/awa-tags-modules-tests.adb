-----------------------------------------------------------------------
--  awa-tags-modules-tests -- Unit tests for tags module
--  Copyright (C) 2013, 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Test_Caller;
with Util.Beans.Basic;
with Util.Beans.Objects;

with Security.Contexts;

with AWA.Users.Models;
with AWA.Services.Contexts;
with AWA.Tests.Helpers.Users;
with AWA.Tags.Beans;
package body AWA.Tags.Modules.Tests is

   package Caller is new Util.Test_Caller (Test, "Tags.Modules");

   function Create_Tag_List_Bean (Module : in Tag_Module_Access)
                                  return AWA.Tags.Beans.Tag_List_Bean_Access;

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test AWA.Tags.Modules.Add_Tag",
                       Test_Add_Tag'Access);
      Caller.Add_Test (Suite, "Test AWA.Tags.Modules.Remove_Tag",
                       Test_Remove_Tag'Access);
      Caller.Add_Test (Suite, "Test AWA.Tags.Modules.Update_Tags",
                       Test_Remove_Tag'Access);
   end Add_Tests;

   function Create_Tag_List_Bean (Module : in Tag_Module_Access)
                                  return AWA.Tags.Beans.Tag_List_Bean_Access is
      Bean : constant Util.Beans.Basic.Readonly_Bean_Access
        := AWA.Tags.Beans.Create_Tag_List_Bean (Module);
   begin
      return AWA.Tags.Beans.Tag_List_Bean'Class (Bean.all)'Access;
   end Create_Tag_List_Bean;

   --  ------------------------------
   --  Test tag creation.
   --  ------------------------------
   procedure Test_Add_Tag (T : in out Test) is
      Sec_Ctx      : Security.Contexts.Security_Context;
      Context      : AWA.Services.Contexts.Service_Context;
   begin
      AWA.Tests.Helpers.Users.Login (Context, Sec_Ctx, "test-add-tag@test.com");

      declare
         Tag_Manager  : constant Tag_Module_Access := Get_Tag_Module;
         User         : constant AWA.Users.Models.User_Ref := Context.Get_User;
         List         : AWA.Tags.Beans.Tag_List_Bean_Access;
         Cleanup      : Util.Beans.Objects.Object;
      begin
         T.Assert (Tag_Manager /= null, "There is no tag module");

         List := Create_Tag_List_Bean (Tag_Manager);
         Cleanup := Util.Beans.Objects.To_Object (List.all'Access);
         List.Set_Value ("entity_type", Util.Beans.Objects.To_Object (String '("awa_user")));

         --  Create a tag.
         Tag_Manager.Add_Tag (User.Get_Id, "awa_user", "workspaces-create", "user-tag");
         Tag_Manager.Add_Tag (User.Get_Id, "awa_user", "workspaces-create", "user-tag");

         --  Load the list.
         List.Load_Tags (Tag_Manager.Get_Session, User.Get_Id);

         Util.Tests.Assert_Equals (T, 1, Integer (List.Get_Count), "Invalid number of tags");
         T.Assert (not Util.Beans.Objects.Is_Null (Cleanup), "Cleanup instance is null");
      end;
   end Test_Add_Tag;

   --  ------------------------------
   --  Test tag removal.
   --  ------------------------------
   procedure Test_Remove_Tag (T : in out Test) is
      Sec_Ctx      : Security.Contexts.Security_Context;
      Context      : AWA.Services.Contexts.Service_Context;
   begin
      AWA.Tests.Helpers.Users.Login (Context, Sec_Ctx, "test-tag@test.com");

      declare
         Tag_Manager  : constant Tag_Module_Access := Get_Tag_Module;
         User         : constant AWA.Users.Models.User_Ref := Context.Get_User;
         List         : AWA.Tags.Beans.Tag_List_Bean_Access;
         Cleanup      : Util.Beans.Objects.Object;
      begin
         T.Assert (Tag_Manager /= null, "There is no tag module");

         List := Create_Tag_List_Bean (Tag_Manager);
         Cleanup := Util.Beans.Objects.To_Object (List.all'Access);
         List.Set_Value ("entity_type", Util.Beans.Objects.To_Object (String '("awa_user")));

         Tag_Manager.Add_Tag (User.Get_Id, "awa_user", "workspaces-create", "user-tag-1");
         Tag_Manager.Add_Tag (User.Get_Id, "awa_user", "workspaces-create", "user-tag-2");
         Tag_Manager.Add_Tag (User.Get_Id, "awa_user", "workspaces-create", "user-tag-3");

         Tag_Manager.Remove_Tag (User.Get_Id, "awa_user", "workspaces-create", "user-tag-2");
         Tag_Manager.Remove_Tag (User.Get_Id, "awa_user", "workspaces-create", "user-tag-1");

         --  Load the list.
         List.Load_Tags (Tag_Manager.Get_Session, User.Get_Id);
         Util.Tests.Assert_Equals (T, 1, Integer (List.Get_Count), "Invalid number of tags");
         T.Assert (not Util.Beans.Objects.Is_Null (Cleanup), "Cleanup instance is null");
      end;
   end Test_Remove_Tag;

   --  ------------------------------
   --  Test tag creation and removal.
   --  ------------------------------
   procedure Test_Update_Tag (T : in out Test) is
      Sec_Ctx      : Security.Contexts.Security_Context;
      Context      : AWA.Services.Contexts.Service_Context;
   begin
      AWA.Tests.Helpers.Users.Login (Context, Sec_Ctx, "test-tag@test.com");

      declare
         Tag_Manager  : constant Tag_Module_Access := Get_Tag_Module;
         User         : constant AWA.Users.Models.User_Ref := Context.Get_User;
         List         : AWA.Tags.Beans.Tag_List_Bean_Access;
         Cleanup      : Util.Beans.Objects.Object;
         Tags         : Util.Strings.Vectors.Vector;
      begin
         T.Assert (Tag_Manager /= null, "There is no tag module");

         List := Create_Tag_List_Bean (Tag_Manager);
         Cleanup := Util.Beans.Objects.To_Object (List.all'Access);
         List.Set_Value ("entity_type", Util.Beans.Objects.To_Object (String '("awa_user")));
         List.Set_Value ("permission",
                         Util.Beans.Objects.To_Object (String '("workspace-create")));

         --  Add 3 tags.
         Tags.Append ("user-tag-1");
         Tags.Append ("user-tag-2");
         Tags.Append ("user-tag-3");
         List.Set_Added (Tags);

         List.Update_Tags (User.Get_Id);

         --  Load the list.
         List.Load_Tags (Tag_Manager.Get_Session, User.Get_Id);
         Util.Tests.Assert_Equals (T, 3, Integer (List.Get_Count), "Invalid number of tags");

         --  Remove a tag that was not created.
         Tags.Append ("user-tag-4");
         List.Set_Deleted (Tags);

         Tags.Clear;
         Tags.Append ("user-tag-5");
         List.Set_Added (Tags);
         List.Update_Tags (User.Get_Id);

         --  'user-tag-5' is the only tag that should exist now.
         List.Load_Tags (Tag_Manager.Get_Session, User.Get_Id);
         Util.Tests.Assert_Equals (T, 1, Integer (List.Get_Count), "Invalid number of tags");

         T.Assert (not Util.Beans.Objects.Is_Null (Cleanup), "Cleanup instance is null");
      end;
   end Test_Update_Tag;

end AWA.Tags.Modules.Tests;
