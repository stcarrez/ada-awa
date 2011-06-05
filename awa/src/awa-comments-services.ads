-----------------------------------------------------------------------
--  awa-comments-services -- Comments management
--  Copyright (C) 2009, 2010, 2011 Stephane Carrez
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
with AWA.Comments.Models;
with AWA.Users.Models;
with AWA.Modules;
with ADO;
with ADO.Objects;
package AWA.Comments.Services is

   use AWA.Comments.Models;

   NAME : constant String := "Comment_Service";

   type Comment_Manager is new AWA.Modules.Module_Manager with private;
   type Comment_Manager_Access is access all Comment_Manager'Class;

   --  Create a user in the database with the given user information and
   --  the associated email address.  Verify that no such user already exist.
   --  Raises User_Exist exception if a user with such email is already registered.
   procedure Create_Comment (Model   : in Comment_Manager;
                             Comment : in out Comment_Ref'Class;
                             User    : in AWA.Users.Models.User_Ref'Class);

   procedure Create_Comment (Model   : in Comment_Manager;
                             Entity  : in ADO.Objects.Object_Key;
                             Message : in String;
                             User    : in AWA.Users.Models.User_Ref'Class;
                             Result  : out ADO.Identifier);

   procedure Find_Comment (Model   : in Comment_Manager;
                           Id      : in ADO.Identifier;
                           Comment : in out Comment_Ref'Class);

   --  Delete the comment with the given identifier.
   procedure Delete_Comment (Model   : in Comment_Manager;
                             Id      : in ADO.Identifier);

private

   type Comment_Manager is new AWA.Modules.Module_Manager with null record;

end AWA.Comments.Services;
