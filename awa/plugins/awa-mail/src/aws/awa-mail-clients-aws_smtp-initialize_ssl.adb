-----------------------------------------------------------------------
--  awa-mail-clients-aws_smtp-initialize -- Initialize SMTP client with SSL support
--  Copyright (C) 2016, 2022 Stephane Carrez
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

separate (AWA.Mail.Clients.AWS_SMTP)

procedure Initialize (Client : in out AWS_Mail_Manager'Class;
                      Props  : in Util.Properties.Manager'Class) is

   Server : constant String := Props.Get (Name => "smtp.host", Default => "localhost");
   User   : constant String := Props.Get (Name => "smtp.user", Default => "");
   Passwd : constant String := Props.Get (Name => "smtp.password", Default => "");
   Secure : constant String := Props.Get (Name => "smtp.ssl", Default => "0");
begin
   Client.Secure := Secure in "1" | "yes" | "true";
   if User'Length > 0 then
      Client.Creds  := AWS.SMTP.Authentication.Plain.Initialize (User, Passwd);
      Client.Server := AWS.SMTP.Client.Initialize (Server_Name => Server,
                                                   Port        => Client.Port,
                                                   Secure      => Client.Secure,
                                                   Credential  => Client.Creds'Unchecked_Access);
   else
      Client.Server := AWS.SMTP.Client.Initialize (Server_Name => Server,
                                                   Port        => Client.Port,
                                                   Secure      => Client.Secure);
   end if;
end Initialize;
