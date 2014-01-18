-----------------------------------------------------------------------
--  awa-comments-modules-tests -- Unit tests for comments service
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
