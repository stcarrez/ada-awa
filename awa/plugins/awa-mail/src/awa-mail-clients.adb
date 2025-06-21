-----------------------------------------------------------------------
--  awa-mail-client -- Mail client interface
--  Copyright (C) 2012 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package body AWA.Mail.Clients is

   function Factory (Name  : in String;
                     Props : in Util.Properties.Manager'Class)
                     return Mail_Manager_Access is separate;

end AWA.Mail.Clients;
