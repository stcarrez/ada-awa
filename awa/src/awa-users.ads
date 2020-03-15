-----------------------------------------------------------------------
--  awa-users -- Users module
--  Copyright (C) 2009, 2010, 2011, 2015, 2018, 2020 Stephane Carrez
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

--  = Users Module =
--  The `users` module manages the creation, update, removal and authentication
--  of users in an application.  The module provides the foundations for user
--  management in a web application.
--
--  A user can register himself by using a subscription form.  In that case,
--  a verification mail is sent and the user has to follow the verification
--  link defined in the mail to finish the registration process.  The user
--  will authenticate using a password.
--
--  A user can also use an OpenID account and be automatically registered.
--
--  A user can have one or several permissions that allow to protect the
--  application data.  User permissions are managed by the `Permissions.Module`.
--
--  @include awa-users-modules.ads
--
--  == Configuration ==
--  The *users* module uses a set of configuration properties to configure
--  the OpenID integration.
--
--  @include-config users.xml
--
--  @include awa-users-beans.ads
--
--  == Data model ==
--  [images/awa_users_model.png]
--
package AWA.Users is

   pragma Preelaborate;

end AWA.Users;
