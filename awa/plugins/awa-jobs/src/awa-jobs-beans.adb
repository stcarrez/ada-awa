-----------------------------------------------------------------------
--  awa-jobs-beans -- AWA Jobs Ada Beans
--  Copyright (C) 2012, 2013, 2015 Stephane Carrez
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

with AWA.Events.Action_Method;
package body AWA.Jobs.Beans is

   package Execute_Binding is
     new AWA.Events.Action_Method.Bind (Bean   => Process_Bean,
                                        Method => Execute,
                                        Name   => "execute");

   Process_Binding : aliased constant Util.Beans.Methods.Method_Binding_Array
     := (1 => Execute_Binding.Proxy'Access);

   --  ------------------------------
   --  Get the value identified by the name.
   --  ------------------------------
   overriding
   function Get_Value (From : in Process_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      return AWA.Jobs.Services.Get_Parameter (From.Job, Name);
   end Get_Value;

   --  ------------------------------
   --  Set the value identified by the name.
   --  ------------------------------
   overriding
   procedure Set_Value (From  : in out Process_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      null;
   end Set_Value;

   --  ------------------------------
   --  This bean provides some methods that can be used in a Method_Expression
   --  ------------------------------
   overriding
   function Get_Method_Bindings (From : in Process_Bean)
                                 return Util.Beans.Methods.Method_Binding_Array_Access is
      pragma Unreferenced (From);
   begin
      return Process_Binding'Access;
   end Get_Method_Bindings;

   --  ------------------------------
   --  Execute the job described by the event.
   --  ------------------------------
   procedure Execute (Bean    : in out Process_Bean;
                      Event   : in AWA.Events.Module_Event'Class) is
   begin
      AWA.Jobs.Services.Execute (Event, Bean.Job);
   end Execute;

   --  ------------------------------
   --  Create the job process bean instance.
   --  ------------------------------
   function Create_Process_Bean (Module : in AWA.Jobs.Modules.Job_Module_Access)
                                 return Util.Beans.Basic.Readonly_Bean_Access is
      Result : constant Process_Bean_Access := new Process_Bean;
   begin
      Result.Module := Module;
      return Result.all'Access;
   end Create_Process_Bean;

end AWA.Jobs.Beans;
