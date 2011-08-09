-----------------------------------------------------------------------
--  awa-blogs-services -- Blogs and post management
--  Copyright (C) 2011 Stephane Carrez
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
with AWA.Modules;
with ADO;

--  The <b>Blogs.Services</b> package defines the service and operations to
--  create, update and delete a post.
package AWA.Blogs.Services is

   NAME : constant String := "Blog_Service";

   --  Exception raised when a post cannot be found.
   Not_Found : exception;

   type Blog_Service is new AWA.Modules.Module_Manager with private;
   type Blog_Service_Access is access all Blog_Service'Class;

   --  Create a new post associated with the given blog identifier.
   procedure Create_Post (Model   : in Blog_Service;
                          Blog_Id : in ADO.Identifier;
                          Title   : in String;
                          URI     : in String;
                          Text    : in String;
                          Result  : out ADO.Identifier);

   --  Update the post title and text associated with the blog post identified by <b>Post</b>.
   procedure Update_Post (Model   : in Blog_Service;
                          Post_Id : in ADO.Identifier;
                          Title   : in String;
                          Text    : in String);

   --  Delete the post identified by the given identifier.
   procedure Delete_Post (Model   : in Blog_Service;
                          Id      : in ADO.Identifier);

private

   type Blog_Service is new AWA.Modules.Module_Manager with null record;

end AWA.Blogs.Services;
