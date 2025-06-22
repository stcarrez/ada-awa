-----------------------------------------------------------------------
--  awa-tags -- Tags management
--  Copyright (C) 2013, 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

--  = Tags Module =
--  The `Tags` module allows to associate general purpose tags to any database entity.
--  It provides a JSF component that allows to insert easily a list of tags in a page and
--  in a form.  An application can use the bean types defined in `AWA.Tags.Beans`
--  to define the tags and it will use the `awa:tagList` component to display them.
--  A tag cloud is also provided by the `awa:tagCloud` component.
--
--  @include awa-tags-modules.ads
--  @include awa-tags-beans.ads
--  @include awa-tags-components.ads
--
--  == Queries ==
--  @include-query tag-queries.xml
--
--  == Data model ==
--  The database model is generic and it uses the `Entity_Type` provided by
--  Ada Database Objects to associate a tag to entities stored in different
--  tables.  The `Entity_Type` identifies the database table and the stored identifier
--  in `for_entity_id` defines the entity in that table.
--
--  [images/awa_tags_model.png]
--
package AWA.Tags is

   pragma Pure;

end AWA.Tags;
