-----------------------------------------------------------------------
--  awa-comments-modules-tests -- Unit tests for comments service
--  Copyright (C) 2014 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Beans.Objects;
with Util.Tests;
with AWA.Tests;

package AWA.Comments.Modules.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new AWA.Tests.Test with record
      Manager : AWA.Comments.Modules.Comment_Module_Access;
   end record;

   procedure List_Comments (T    : in out Test;
                            User : in ADO.Identifier;
                            Into : out Util.Beans.Objects.Object);

   --  Create a comment and return the list of comments before and after the creation.
   procedure Create_Comment (T : in out Test;
                             Status : in AWA.Comments.Models.Status_Type;
                             Before : out Util.Beans.Objects.Object;
                             After  : out Util.Beans.Objects.Object;
                             Id     : out ADO.Identifier);

   --  Test comment creation (PUBLISHED).
   procedure Test_Create_Published_Comment (T : in out Test);

   --  Test comment creation (WAITING).
   procedure Test_Create_Waiting_Comment (T : in out Test);

   --  Test comment removal.
   procedure Test_Remove_Comment (T : in out Test);

   --  Test comment update.
   procedure Test_Update_Comment (T : in out Test);

   --  Test comment publication through the publish operation bean.
   procedure Test_Publish_Comment (T : in out Test);

end AWA.Comments.Modules.Tests;
