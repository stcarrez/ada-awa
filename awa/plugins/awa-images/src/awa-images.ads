-----------------------------------------------------------------------
--  awa-images -- Image module
--  Copyright (C) 2012, 2016, 2018, 2020 Stephane Carrez
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

--  = Images Module =
--  The `images` module is an extension of the [Storages Module] that
--  identifies images and provides thumbnails as well as resizing of
--  the original image.
--
--  The `images` module uses several other modules:
--
--  * the [Storage Module] to store and manage image content,
--  * the [Jobs Module] to schedule image thumbnail generation.
--
--  @include awa-images-modules.ads
--  @include awa-images-beans.ads
--
--  == Queries ==
--  @include-query image-info.xml
--  @include-query image-list.xml
--
--  == Data model ==
--  [images/awa_images_model.png]
--
package AWA.Images is

   pragma Pure;

end AWA.Images;
