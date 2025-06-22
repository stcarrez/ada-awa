-----------------------------------------------------------------------
--  awa-events-configs -- Event configuration
--  Copyright (C) 2012, 2013, 2017, 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Strings.Unbounded;

with Util.Beans.Objects;
with Util.Serialize.Mappers;
with EL.Expressions;
with EL.Beans;
with EL.Contexts;
with ADO.Sessions;

with AWA.Events.Queues;
with AWA.Events.Services;
package AWA.Events.Configs is

   --  ------------------------------
   --  Event Config Controller
   --  ------------------------------
   type Controller_Config is record
      Name       : Util.Beans.Objects.Object;
      Queue      : AWA.Events.Queues.Queue_Ref;
      Queue_Type : Util.Beans.Objects.Object;
      Prop_Name  : Util.Beans.Objects.Object;
      Params     : EL.Beans.Param_Vectors.Vector;
      Priority   : Integer;
      Count      : Integer;
      Manager    : AWA.Events.Services.Event_Manager_Access;
      Action     : EL.Expressions.Method_Expression;
      Properties : EL.Beans.Param_Vectors.Vector;
      Context    : EL.Contexts.ELContext_Access;
      Session    : ADO.Sessions.Session;
      Match      : Ada.Strings.Unbounded.Unbounded_String;
   end record;
   type Controller_Config_Access is access all Controller_Config;

   type Config_Fields is (FIELD_ON_EVENT, FIELD_NAME, FIELD_QUEUE_NAME, FIELD_ACTION,
                          FIELD_PROPERTY_NAME, FIELD_QUEUE, FIELD_TYPE,
                          FIELD_PROPERTY_VALUE,
                          FIELD_DISPATCHER, FIELD_DISPATCHER_QUEUE, FIELD_DISPATCHER_PRIORITY,
                          FIELD_DISPATCHER_COUNT);

   --  Set the configuration value identified by <b>Value</b> after having parsed
   --  the element identified by <b>Field</b>.
   procedure Set_Member (Into  : in out Controller_Config;
                         Field : in Config_Fields;
                         Value : in Util.Beans.Objects.Object);

private

   procedure Add_Mapping (Mapper : in out Util.Serialize.Mappers.Processing;
                          Config : in Controller_Config_Access);

end AWA.Events.Configs;
