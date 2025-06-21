-----------------------------------------------------------------------
--  awa-blogs -- Blogs module
--  Copyright (C) 2011, 2014, 2015, 2018, 2020 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

--  = Blogs Module =
--  The `blogs` module is a small blog application which allows users
--  to publish articles.  A user may own several blogs, each blog having
--  a name and its own base URI.  Within a blog, the user may write articles
--  and publish them.  Once published, the articles are visible to
--  anonymous users.
--
--  The `blogs` module uses several other modules:
--
--  * the [Counters Module] to track page display counter to a blog post,
--  * the [Tags Module] to associate one or several tags to a blog post,
--  * the [Comments Module] to allow users to write comments on a blog post,
--  * the [Images Module] to easily add images in blog post.
--
--  @include awa-blogs-modules.ads
--  @include awa-blogs-beans.ads
--  @include-bean blogs.xml
--  @include-bean blog-admin-post-list.xml
--  @include-bean blog-post-list.xml
--  @include-bean blog-comment-list.xml
--  @include-bean blog-list.xml
--  @include-bean blog-tags.xml
--
--  == Queries ==
--  @include-query blog-admin-post-list.xml
--  @include-query blog-post-list.xml
--  @include-query blog-comment-list.xml
--  @include-query blog-list.xml
--  @include-query blog-tags.xml
--  @include-query blog-images.xml
--  @include-query blog-images-info.xml
--  @include-query blog-stat.xml
--
--  == Data model ==
--  [images/awa_blogs_model.png]
--
package AWA.Blogs is

   pragma Pure;

end AWA.Blogs;
