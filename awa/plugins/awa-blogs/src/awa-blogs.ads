-----------------------------------------------------------------------
--  awa-blogs -- Blogs module
--  Copyright (C) 2011, 2014, 2015 Stephane Carrez
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

--  == Introduction ==
--  The *blogs* plugin is a small blog application which allows users to publish articles.
--  A user may own several blogs, each blog having a name and its own base URI.  Within a blog,
--  the user may write articles and publish them.  Once published, the articles are visible to
--  anonymous users.
--
--  The *blogs* plugin uses the [AWA_Tags] and [AWA_Comments] modules to allow to associate
--  tags to a post and allow users to comment on the articles.
--
--  == Model ==
--  [images/awa_blogs_model.png]
--
--  @include awa-blogs-modules.ads
--  @include awa-blogs-beans.ads
--
--  == Ada Beans ==
--  @include blogs.xml
--
--  == Queries ==
--  @include blog-admin-post-list.xml
--  @include blog-post-list.xml
--  @include blog-comment-list.xml
--  @include blog-list.xml
--  @include blog-tags.xml
--
package AWA.Blogs is

   pragma Pure;

end AWA.Blogs;
