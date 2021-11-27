-----------------------------------------------------------------------
--  awa-mail-clients-factory -- Factory to create the mail manager
--  Copyright (C) 2012 Stephane Carrez
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
with AWA.Mail.Clients.Files;

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
   else
      return null;
   end if;
end Factory;
