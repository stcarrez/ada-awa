-----------------------------------------------------------------------
--  awa-jobs-beans -- AWA Jobs Ada Beans
--  Copyright (C) 2012, 2013, 2015 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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
