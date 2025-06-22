-----------------------------------------------------------------------
--  awa-workspaces -- Module workspaces
--  Copyright (C) 2011, 2012, 2015, 2017, 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

--  = Workspaces Module =
--  The *workspaces* plugin defines a workspace area for other plugins.
--  The workspace is intended to link together all the data objects that an application
--  manages for a given user or group of users.  A workspace is a possible mechanism
--  to provide and implement multi-tenancy in a web application.  By using the workspace plugin,
--  application data from different customers can be kept separate from each other in the
--  same database.
--
--  @include awa-workspaces-modules.ads
--
--  == Ada Beans ==
--  @include workspaces.xml
--
--  == Data Model ==
--  [images/awa_workspace_model.png]
--
package AWA.Workspaces is
end AWA.Workspaces;
