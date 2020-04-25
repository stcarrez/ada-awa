-----------------------------------------------------------------------
--  awa-mail-beans -- Beans for mail module
--  Copyright (C) 2012, 2020 Stephane Carrez
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

with Ada.Strings.Unbounded;

with Util.Beans.Basic;
with Util.Beans.Objects;
with Util.Beans.Methods;
with Util.Beans.Objects.Maps;

with AWA.Events;
with AWA.Mail.Modules;
package AWA.Mail.Beans is

   type Mail_Bean is new Util.Beans.Basic.Bean
     and Util.Beans.Methods.Method_Bean with record
      Module   : AWA.Mail.Modules.Mail_Module_Access := null;
      Props    : Util.Beans.Objects.Maps.Map;
      Template : Ada.Strings.Unbounded.Unbounded_String;
      Params   : Util.Beans.Objects.Maps.Map;
   end record;
   type Mail_Bean_Access is access all Mail_Bean'Class;

   --  Get the value identified by the name.
   overriding
   function Get_Value (From : in Mail_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the value identified by the name.
   overriding
   procedure Set_Value (From  : in out Mail_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   --  This bean provides some methods that can be used in a Method_Expression
   overriding
   function Get_Method_Bindings (From : in Mail_Bean)
                                 return Util.Beans.Methods.Method_Binding_Array_Access;

   --  Format and send the mail.
   procedure Send_Mail (Bean    : in out Mail_Bean;
                        Event   : in AWA.Events.Module_Event'Class);

   --  Create the mail bean instance.
   function Create_Mail_Bean (Module : in AWA.Mail.Modules.Mail_Module_Access)
                                    return Util.Beans.Basic.Readonly_Bean_Access;

end AWA.Mail.Beans;
