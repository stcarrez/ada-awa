-----------------------------------------------------------------------
--  awa-wikis -- Wiki module
--  Copyright (C) 2011, 2018 Stephane Carrez
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
