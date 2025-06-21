-----------------------------------------------------------------------
--  awa-changelogs -- Changelog module
--  Copyright (C) 2014, 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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
