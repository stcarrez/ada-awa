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
with Util.Strings;

with AWA.Events.Action_Method;
package body AWA.Mail.Beans is

   package Send_Mail_Binding is
     new AWA.Events.Action_Method.Bind (Bean   => Mail_Bean,
                                        Method => Send_Mail,
                                        Name   => "send");

   Mail_Bean_Binding : aliased constant Util.Beans.Methods.Method_Binding_Array
     := (1 => Send_Mail_Binding.Proxy'Access);

   --  ------------------------------
   --  Get the value identified by the name.
   --  ------------------------------
   overriding
   function Get_Value (From : in Mail_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = "template" then
         return Util.Beans.Objects.To_Object (From.Template);
      else
         declare
            Pos : constant Util.Beans.Objects.Maps.Cursor := From.Props.Find (Name);
         begin
            if Util.Beans.Objects.Maps.Has_Element (Pos) then
               return Util.Beans.Objects.Maps.Element (Pos);
            else
               return Util.Beans.Objects.Null_Object;
            end if;
         end;
      end if;
   end Get_Value;

   --  ------------------------------
   --  Set the value identified by the name.
   --  ------------------------------
   overriding
   procedure Set_Value (From  : in out Mail_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if Name = "template" then
         From.Template := Util.Beans.Objects.To_Unbounded_String (Value);
      elsif Util.Strings.Starts_With (Name, "request.") then
         From.Params.Include (Name (Name'First + 8 .. Name'Last), Value);
      else
         From.Props.Include (Name, Value);
      end if;
   end Set_Value;

   --  ------------------------------
   --  This bean provides some methods that can be used in a Method_Expression
   --  ------------------------------
   overriding
   function Get_Method_Bindings (From : in Mail_Bean)
                                 return Util.Beans.Methods.Method_Binding_Array_Access is
      pragma Unreferenced (From);
   begin
      return Mail_Bean_Binding'Access;
   end Get_Method_Bindings;

   --  ------------------------------
   --  Format and send the mail.
   --  ------------------------------
   procedure Send_Mail (Bean    : in out Mail_Bean;
                        Event   : in AWA.Events.Module_Event'Class) is
      use Ada.Strings.Unbounded;
   begin
      Bean.Module.Send_Mail (To_String (Bean.Template), Bean.Props,
                             Bean.Params, Event);
   end Send_Mail;

   --  ------------------------------
   --  Create the mail bean instance.
   --  ------------------------------
   function Create_Mail_Bean (Module : in AWA.Mail.Modules.Mail_Module_Access)
                              return Util.Beans.Basic.Readonly_Bean_Access is
      Result : constant Mail_Bean_Access := new Mail_Bean;
   begin
      Result.Module := Module;
      return Result.all'Access;
   end Create_Mail_Bean;

end AWA.Mail.Beans;
