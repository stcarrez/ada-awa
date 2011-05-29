-----------------------------------------------------------------------
--  awa -- Ada Web Application
--  Copyright (C) 2009, 2010, 2011 Stephane Carrez
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

with ASF.Requests;
with ASF.Responses;
with ASF.Server;

package body AWA.Modules is

   procedure Initialize (Manager : in out Module_Manager;
                         Module  : in AWA.Modules.Module'Class) is
   begin
      Manager.Module := Module.Self;
   end Initialize;

   function Get_Value (Manager : in Module_Manager;
                       Name    : in String) return Util.Beans.Objects.Object is
      pragma Unreferenced (Manager, Name);
   begin
      return Util.Beans.Objects.Null_Object;
   end Get_Value;

   --  Module manager
   --
   --  ------------------------------
   --  Get the database connection for reading
   --  ------------------------------
   function Get_Session (Manager : Module_Manager)
                            return ADO.Sessions.Session is
   begin
      return Manager.Module.Get_Session;
   end Get_Session;

   --  ------------------------------
   --  Get the database connection for writing
   --  ------------------------------
   function Get_Master_Session (Manager : Module_Manager)
                                   return ADO.Sessions.Master_Session is
   begin
      return Manager.Module.Get_Master_Session;
   end Get_Master_Session;

   --  ------------------------------
   --  Send the event to the module.  The module identified by <b>To</b> is
   --  found and the event is posted on its event channel.
   --  ------------------------------
   procedure Send_Event (Manager : in Module_Manager;
                         To      : in String;
                         Content : in ASF.Events.Modules.Module_Event'Class) is
   begin
      Manager.Module.Send_Event (To, Content);
   end Send_Event;

   procedure Initialize (Plugin : in out Module;
                         App    : access ASF.Applications.Main.Application'Class) is
   begin
      Plugin.Self := Plugin'Unchecked_Access;
      ASF.Modules.Module (Plugin).Initialize (App);
      Plugin.Awa_App := AWA.Applications.Application'Class (App.all)'Unchecked_Access;
   end Initialize;

   --  ------------------------------
   --  Get the database connection for reading
   --  ------------------------------
   function Get_Session (Manager : Module)
                            return ADO.Sessions.Session is
   begin
      return Manager.Awa_App.Get_Session;
   end Get_Session;

   --  ------------------------------
   --  Get the database connection for writing
   --  ------------------------------
   function Get_Master_Session (Manager : Module)
                                return ADO.Sessions.Master_Session is
   begin
      return Manager.Awa_App.Get_Master_Session;
   end Get_Master_Session;

   --  Get per request manager => look in Request
   --  Get per session manager => look in Request.Get_Session
   --  Get per application manager => look in Application
   --  Get per pool manager => look in pool attached to Application
   function Get_Manager return Manager_Type_Access is

      Value : Util.Beans.Objects.Object;

      procedure Process (Request  : in out ASF.Requests.Request'Class;
                         Response : in out ASF.Responses.Response'Class) is
         pragma Unreferenced (Response);
      begin
         Value := Request.Get_Attribute (Name);
         if Util.Beans.Objects.Is_Null (Value) then
            declare
               M : constant Manager_Type_Access := new Manager_Type;
            begin
               Value := Util.Beans.Objects.To_Object (M.all'Access);
               Request.Set_Attribute (Name, Value);
            end;
         end if;
      end Process;

   begin
      ASF.Server.Update_Context (Process'Access);
      if Util.Beans.Objects.Is_Null (Value) then
         return null;
      end if;
      declare
         B : constant access Util.Beans.Basic.Readonly_Bean'Class
           := Util.Beans.Objects.To_Bean (Value);
      begin
         if not (B.all in Manager_Type'Class) then
            return null;
         end if;
         return Manager_Type'Class (B.all)'Unchecked_Access;
      end;
   end Get_Manager;

end AWA.Modules;
