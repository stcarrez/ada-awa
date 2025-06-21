-----------------------------------------------------------------------
--  awa-mail-clients-factory -- Factory to create the mail manager
--  Copyright (C) 2012 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with AWA.Mail.Clients.Files;
with AWA.Mail.Clients.AWS_SMTP;

separate (AWA.Mail.Clients)

--  ------------------------------
--  Factory to create the mail manager.  The mail manager implementation is identified by
--  the <b>Name</b>.  It is configured according to the properties defined in <b>Props</b>.
--  Returns null if the mail manager identified by the name is not supported.
--  ------------------------------
function Factory (Name  : in String;
                  Props : in Util.Properties.Manager'Class) return Mail_Manager_Access is
begin
   if Name = AWA.Mail.Clients.Files.NAME then
      return AWA.Mail.Clients.Files.Create_Manager (Props);
   elsif Name = AWA.Mail.Clients.AWS_SMTP.NAME then
      return AWA.Mail.Clients.AWS_SMTP.Create_Manager (Props);
   else
      return null;
   end if;
end Factory;
