-----------------------------------------------------------------------
--  awa-mail-clients-aws_smtp-initialize -- Initialize SMTP client with SSL support
--  Copyright (C) 2016, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

separate (AWA.Mail.Clients.AWS_SMTP)

procedure Initialize (Client : in out AWS_Mail_Manager'Class;
                      Props  : in Util.Properties.Manager'Class) is

   Server : constant String := Props.Get (Name => "smtp.host", Default => "localhost");
   User   : constant String := Props.Get (Name => "smtp.user", Default => "");
   Passwd : constant String := Props.Get (Name => "smtp.password", Default => "");
   Secure : constant String := Props.Get (Name => "smtp.ssl", Default => "0");
   Security : AWS.SMTP.Secure_Connection;
begin
   Client.Secure := Secure in "1" | "yes" | "true";
   Security := (if Client.Secure then AWS.SMTP.TLS else AWS.SMTP.No);
   if User'Length > 0 then
      Client.Creds  := AWS.SMTP.Authentication.Plain.Initialize (User, Passwd);
      Client.Server := AWS.SMTP.Client.Initialize (Server_Name => Server,
                                                   Port        => Client.Port,
                                                   Security    => Security,
                                                   Credential  => Client.Creds'Unchecked_Access);
   else
      Client.Server := AWS.SMTP.Client.Initialize (Server_Name => Server,
                                                   Port        => Client.Port,
                                                   Security    => Security);
   end if;
end Initialize;
