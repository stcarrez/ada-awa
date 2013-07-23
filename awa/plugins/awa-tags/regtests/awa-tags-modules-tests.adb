-----------------------------------------------------------------------
--  awa-tags-modules-tests -- Unit tests for tags module
--  Copyright (C) 2013 Stephane Carrez
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

with Util.Test_Caller;
with Util.Beans.Basic;
with Util.Beans.Objects;

with Security.Contexts;

with AWA.Users.Models;
with AWA.Services.Contexts;
with AWA.Tests.Helpers.Users;
with AWA.Tags.Beans;
package body AWA.Tags.Modules.Tests is

   use Util.Tests;
   use ADO;

   package Caller is new Util.Test_Caller (Test, "Tags.Modules");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test AWA.Tags.Modules.Add_Tag",
                       Test_Add_Tag'Access);
      Caller.Add_Test (Suite, "Test AWA.Tags.Modules.Remove_Tag",
                       Test_Remove_Tag'Access);
   end Add_Tests;

   function Create_Tag_List_Bean (Module : in Tag_Module_Access)
                                  return AWA.Tags.Beans.Tag_List_Bean_Access is
      Bean : Util.Beans.Basic.Readonly_Bean_Access := AWA.Tags.Beans.Create_Tag_List_Bean (Module);
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
      end;
   end Test_Remove_Tag;

end AWA.Tags.Modules.Tests;
