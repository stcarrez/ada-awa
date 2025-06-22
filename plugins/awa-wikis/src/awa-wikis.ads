-----------------------------------------------------------------------
--  awa-wikis -- Wiki module
--  Copyright (C) 2011, 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

--  = Wikis Module =
--  The `Wikis` module provides a complete wiki system which allows users to create
--  their own wiki environment with their wiki pages.
--
--  @include awa-wikis-modules.ads
--
--  @include awa-wikis-beans.ads
--
--  == Queries ==
--  @include-query wiki-page.xml
--  @include-query wiki-pages.xml
--  @include-query wiki-history.xml
--  @include-query wiki-list.xml
--  @include-query wiki-images.xml
--  @include-query wiki-images-info.xml
--  @include-query wiki-stat.xml
--
--  == Data model ==
--  [images/awa_wikis_model.png]
--
package AWA.Wikis is

   pragma Preelaborate;

end AWA.Wikis;
