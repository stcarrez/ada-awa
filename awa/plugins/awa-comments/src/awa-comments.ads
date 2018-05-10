-----------------------------------------------------------------------
--  awa-comments -- Comments module
--  Copyright (C) 2009, 2010, 2011, 2014, 2015, 2018 Stephane Carrez
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

--  = Comments Module =
--  The <b>Comments</b> module is a general purpose module that allows to associate user comments
--  to any database entity.  The module defines several bean types that allow to display a list
--  of comments or edit and publish a new comment.
--
--  == Model ==
--  The database model is generic and it uses the <tt>Entity_Type</tt> provided by
--  [https://github.com/stcarrez/ada-ado ADO] to associate a comment to entities stored in
--  different tables.  The <tt>Entity_Type</tt> identifies the database table and the stored
--  identifier in <tt>for_entity_id</tt> defines the entity in that table.
--
--  [images/awa_comments_model.png]
--
--  @include awa-comments-modules.ads
--  @include awa-comments-beans.ads
--  @include comment-queries.xml
--
package AWA.Comments is

end AWA.Comments;
