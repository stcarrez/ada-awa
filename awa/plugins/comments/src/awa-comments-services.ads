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

--  The <b>Comments.Services</b> package defines the service and operations to
--  create, list, get and remove a comment.
package AWA.Comments.Services is

   use AWA.Comments.Models;

   NAME : constant String := "Comment_Service";

   type Comment_Service is new AWA.Modules.Module_Manager with private;
   type Comment_Service_Access is access all Comment_Service'Class;

   --  Create a comment associated with the given database entity.
   --  The user must have permission to add comments on the given entity.
   procedure Create_Comment (Model   : in Comment_Service;
                             Entity  : in ADO.Objects.Object_Key;
                             Message : in String;
                             User    : in AWA.Users.Models.User_Ref'Class;
                             Result  : out ADO.Identifier);

   procedure Find_Comment (Model   : in Comment_Service;
                           Id      : in ADO.Identifier;
                           Comment : in out Comment_Ref'Class);

   --  Delete the comment identified by the given identifier.
   procedure Delete_Comment (Model   : in Comment_Service;
                             Id      : in ADO.Identifier);

private

   type Comment_Service is new AWA.Modules.Module_Manager with null record;

end AWA.Comments.Services;
