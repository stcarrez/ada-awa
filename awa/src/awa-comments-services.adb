-----------------------------------------------------------------------
--  awa-comments-logic -- Comments management
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

with AWA.Services.Contexts;
with ADO.Sessions;

with Ada.Calendar;

with Util.Log.Loggers;

package body AWA.Comments.Services is

   use Util.Log;
   use ADO.Sessions;
   use AWA.Services;

   Log : constant Loggers.Logger := Loggers.Create ("AWA.Comments.Services");

   --  Create a user in the database with the given user information and
   --  the associated email address.  Verify that no such user already exist.
   --  Raises User_Exist exception if a user with such email is already registered.
   procedure Create_Comment (Model   : in Comment_Manager;
                             Comment : in out Comment_Ref'Class;
                             User    : in AWA.Users.Models.User_Ref'Class) is
   begin
      null;
   end Create_Comment;

   --  Create a comment associated with the given database entity.
   --  The user must have permission to add comments on the given entity.
   --
   --  select from asset inner join acl
   --    on asset.collection_id = acl.entity_id and acl.entity_type = :entity_type
   --    where asset.id = :entity_id and acl.user_id = :user_id
   procedure Create_Comment (Model   : in Comment_Manager;
                             Entity  : in ADO.Objects.Object_Key;
                             Message : in String;
                             User    : in AWA.Users.Models.User_Ref'Class;
                             Result  : out ADO.Identifier) is
      Ctx       : constant Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      DB        : Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
      Comment   : Comment_Ref;
      Entity_Id : constant ADO.Identifier := ADO.Objects.Get_Value (Entity);
   begin
      Log.Info ("Create comment for user {0}", String'(User.Get_Name));

--        AWA.Permissions.Module.Check (Entity => Entity, Permission => CREATE_COMMENT);
      Ctx.Start;
      Comment.Set_Message (Message);
      Comment.Set_Entity_Id (Integer (Entity_Id));
      Comment.Set_User (User);
      Comment.Set_Date (Ada.Calendar.Clock);
      Comment.Save (DB);
      Ctx.Commit;
      Result := Comment.Get_Id;

      Log.Info ("Comment {0} created", ADO.Identifier'Image (Result));
   end Create_Comment;

   procedure Find_Comment (Model   : in Comment_Manager;
                           Id      : in ADO.Identifier;
                           Comment : in out Comment_Ref'Class) is
   begin
      null;
   end Find_Comment;

   --  Delete the comment with the given identifier.
   procedure Delete_Comment (Model   : in Comment_Manager;
                             Id      : in ADO.Identifier) is
   begin
      null;
   end Delete_Comment;

end AWA.Comments.Services;
