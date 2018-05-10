-----------------------------------------------------------------------
--  awa-changelogs -- Changelog module
--  Copyright (C) 2014, 2018 Stephane Carrez
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

--  = Changelogs Module =
--  The <b>Changelog</b> module associate logs produced by users to any database entity.
--  It is intended to be used to track changes and produce a change log for actions performed
--  by users.  The change log entry can be associated with specific database entities so
--  that each database entity can have its own change log history.
--
--  == Model ==
--  [http://ada-awa.googlecode.com/svn/wiki/awa_changelogs_model.png]
--
--  @include awa-changelogs-modules.ads
--  @include awa-changelogs-beans.ads
package AWA.Changelogs is

   pragma Pure;

end AWA.Changelogs;
