-----------------------------------------------------------------------
--  awa-comments -- Comments module
--  Copyright (C) 2009, 2010, 2011, 2014, 2015, 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

--  = Comments Module =
--  The `Comments` module is a general purpose module that allows to associate user comments
--  to any database entity.  The module defines several bean types that allow to display a list
--  of comments or edit and publish a new comment.
--
--  @include awa-comments-modules.ads
--  @include awa-comments-beans.ads
--  @include-query comment-queries.xml
--
--  == Data model ==
--  The database model is generic and it uses the `Entity_Type` provided by
--  Ada Database Objects to associate a comment to entities stored in
--  different tables.  The `Entity_Type` identifies the database table and the stored
--  identifier in `for_entity_id` defines the entity in that table.
--
--  [images/awa_comments_model.png]
--
package AWA.Comments is

end AWA.Comments;
