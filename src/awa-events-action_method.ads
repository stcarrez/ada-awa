-----------------------------------------------------------------------
--  awa-events-action_method -- AWA Events
--  Copyright (C) 2012 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with EL.Methods.Proc_In;

package AWA.Events.Action_Method is
  new EL.Methods.Proc_In (Param1_Type => AWA.Events.Module_Event'Class);
