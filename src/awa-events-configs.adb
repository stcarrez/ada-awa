-----------------------------------------------------------------------
--  awa-events-configs -- Event configuration
--  Copyright (C) 2012, 2013, 2017, 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Serialize.Mappers.Record_Mapper;

with EL.Utils;

package body AWA.Events.Configs is

   use AWA.Events.Queues;

   --  ------------------------------
   --  Set the configuration value identified by <b>Value</b> after having parsed
   --  the element identified by <b>Field</b>.
   --  ------------------------------
   procedure Set_Member (Into  : in out Controller_Config;
                         Field : in Config_Fields;
                         Value : in Util.Beans.Objects.Object) is
      use Ada.Strings.Unbounded;
      use Util.Beans.Objects;
   begin
      case Field is
         when FIELD_NAME =>
            Into.Name := Value;

         when FIELD_TYPE =>
            Into.Queue_Type := Value;

         when FIELD_QUEUE_NAME =>
            declare
               Name : constant String := Util.Beans.Objects.To_String (Value);
            begin
               Into.Queue := Into.Manager.Find_Queue (Name);
            end;

         when FIELD_ACTION =>
            declare
               Expr : constant String := Util.Beans.Objects.To_String (Value);
            begin
               Into.Action := EL.Expressions.Create_Expression (Expr, Into.Context.all);

            exception
               when others =>
                  raise Util.Serialize.Mappers.Field_Error with "Invalid entity type: " & Expr;
            end;

         when FIELD_PROPERTY_NAME =>
            Into.Prop_Name := Value;

         when FIELD_PROPERTY_VALUE =>
            if Util.Beans.Objects.Is_Null (Into.Prop_Name) then
               raise Util.Serialize.Mappers.Field_Error with "Missing property name";
            end if;

            --  Add the new property.
            declare
               Name : constant String := Util.Beans.Objects.To_String (Into.Prop_Name);
               Expr : constant String := Util.Beans.Objects.To_String (Value);
            begin
               EL.Beans.Add_Parameter (Container => Into.Params,
                                       Name      => Name,
                                       Value     => Expr,
                                       Context   => Into.Context.all);
            end;
            Into.Prop_Name := Util.Beans.Objects.Null_Object;

         when FIELD_ON_EVENT =>
            if Util.Beans.Objects.Is_Null (Into.Name) then
               raise Util.Serialize.Mappers.Field_Error with "Missing event name";
            end if;

            declare
               Name  : constant String := Util.Beans.Objects.To_String (Into.Name);
            begin
               Into.Manager.Add_Action (Event  => Name,
                                        Queue  => Into.Queue,
                                        Action => Into.Action,
                                        Params => Into.Params);
            end;
            Into.Name := Util.Beans.Objects.Null_Object;
            Into.Queue := AWA.Events.Queues.Null_Queue;
            Into.Params.Clear;

         when FIELD_QUEUE =>
            --  Create the queue with the given name and properties and add it to the manager.
            declare
               Name  : constant String := Util.Beans.Objects.To_String (Into.Name);
               Kind  : constant String := Util.Beans.Objects.To_String (Into.Queue_Type);
               Queue : constant Queue_Ref := Queues.Create_Queue (Name    => Name,
                                                                  Kind    => Kind,
                                                                  Props   => Into.Params,
                                                                  Context => Into.Context.all);
            begin
               Into.Manager.Add_Queue (Queue);
            end;

            Into.Name       := Util.Beans.Objects.Null_Object;
            Into.Queue_Type := Util.Beans.Objects.Null_Object;
            Into.Params.Clear;

         when FIELD_DISPATCHER_PRIORITY =>
            Into.Priority := To_Integer (EL.Utils.Eval (Value   => Value,
                                                        Context => Into.Context.all));

         when FIELD_DISPATCHER_COUNT =>
            Into.Count := To_Integer (EL.Utils.Eval (Value   => Value,
                                                     Context => Into.Context.all));

         when FIELD_DISPATCHER_QUEUE =>
            declare
               Match : constant String := Util.Beans.Objects.To_String (Value);
            begin
               if Length (Into.Match) > 0 then
                  Append (Into.Match, ",");
               end if;
               Append (Into.Match, Match);
            end;

         when FIELD_DISPATCHER =>
            if Into.Count > 0 then
               Into.Manager.Add_Dispatcher (Match    => To_String (Into.Match),
                                            Count    => Into.Count,
                                            Priority => Into.Priority);
            end if;
            Into.Match := To_Unbounded_String ("");

      end case;
   end Set_Member;

   package Config_Mapper is
     new Util.Serialize.Mappers.Record_Mapper (Element_Type        => Controller_Config,
                                               Element_Type_Access => Controller_Config_Access,
                                               Fields              => Config_Fields,
                                               Set_Member          => Set_Member);

   Event_Mapper : aliased Config_Mapper.Mapper;

   procedure Add_Mapping (Mapper : in out Util.Serialize.Mappers.Processing;
                          Config : in Controller_Config_Access) is
   begin
      Mapper.Add_Mapping ("module", Event_Mapper'Access);
      Config_Mapper.Set_Context (Mapper, Config);
   end Add_Mapping;

begin
   Event_Mapper.Add_Mapping ("queue", FIELD_QUEUE);
   Event_Mapper.Add_Mapping ("queue/@name", FIELD_NAME);
   Event_Mapper.Add_Mapping ("queue/@type", FIELD_TYPE);
   Event_Mapper.Add_Mapping ("queue/property/@name", FIELD_PROPERTY_NAME);
   Event_Mapper.Add_Mapping ("queue/property", FIELD_PROPERTY_VALUE);
   Event_Mapper.Add_Mapping ("on-event", FIELD_ON_EVENT);
   Event_Mapper.Add_Mapping ("on-event/@name", FIELD_NAME);
   Event_Mapper.Add_Mapping ("on-event/@queue", FIELD_QUEUE_NAME);
   Event_Mapper.Add_Mapping ("on-event/property/@name", FIELD_PROPERTY_NAME);
   Event_Mapper.Add_Mapping ("on-event/property", FIELD_PROPERTY_VALUE);
   Event_Mapper.Add_Mapping ("on-event/action", FIELD_ACTION);
   Event_Mapper.Add_Mapping ("dispatcher", FIELD_DISPATCHER);
   Event_Mapper.Add_Mapping ("dispatcher/@name", FIELD_NAME);
   Event_Mapper.Add_Mapping ("dispatcher/queue/@match", FIELD_DISPATCHER_QUEUE);
   Event_Mapper.Add_Mapping ("dispatcher/priority", FIELD_DISPATCHER_PRIORITY);
   Event_Mapper.Add_Mapping ("dispatcher/count", FIELD_DISPATCHER_COUNT);
end AWA.Events.Configs;
