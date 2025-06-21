-----------------------------------------------------------------------
--  awa-jobs-beans -- AWA Jobs Ada Beans
--  Copyright (C) 2012, 2015 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Beans.Objects;
with Util.Beans.Basic;
with Util.Beans.Methods;

with AWA.Events;
with AWA.Jobs.Services;
with AWA.Jobs.Modules;
package AWA.Jobs.Beans is

   --  The <tt>Process_Bean</tt> is the Ada bean that receives the job event and
   --  performs the job action associated with it.
   type Process_Bean is limited new Util.Beans.Basic.Bean
     and Util.Beans.Methods.Method_Bean with private;
   type Process_Bean_Access is access all Process_Bean'Class;

   --  Get the value identified by the name.
   overriding
   function Get_Value (From : in Process_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the value identified by the name.
   overriding
   procedure Set_Value (From  : in out Process_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   --  This bean provides some methods that can be used in a Method_Expression
   overriding
   function Get_Method_Bindings (From : in Process_Bean)
                                 return Util.Beans.Methods.Method_Binding_Array_Access;

   --  Execute the job described by the event.
   procedure Execute (Bean    : in out Process_Bean;
                      Event   : in AWA.Events.Module_Event'Class);

   --  Create the job process bean instance.
   function Create_Process_Bean (Module : in AWA.Jobs.Modules.Job_Module_Access)
                                 return Util.Beans.Basic.Readonly_Bean_Access;

private

   type Process_Bean is limited new Util.Beans.Basic.Bean
     and Util.Beans.Methods.Method_Bean with record
      Module : AWA.Jobs.Modules.Job_Module_Access;
      Job    : AWA.Jobs.Services.Job_Ref;
   end record;

end AWA.Jobs.Beans;
