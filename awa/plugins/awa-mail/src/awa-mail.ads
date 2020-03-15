-----------------------------------------------------------------------
--  awa-mail -- Mail module
--  Copyright (C) 2011, 2017, 2018, 2020 Stephane Carrez
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

--  = Mail Module =
--  The `mail` module allows an application to format and send a mail
--  to users.  This module does not define any web interface.  It provides
--  a set of services and methods to send a mail when an event is
--  received.  All this is done through configuration.  The module
--  defines a set of specific ASF components to format and prepare the
--  email.
--
--  @include awa-mail-modules.ads
package AWA.Mail is

   pragma Pure;

end AWA.Mail;
