-----------------------------------------------------------------------
--  awa-events-queues -- AWA Event Queues
--  Copyright (C) 2012 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Strings.Hash;
with Ada.Containers.Indefinite_Hashed_Maps;

--  The list of queues created for the application.
package AWA.Events.Queues.Maps is
     new Ada.Containers.Indefinite_Hashed_Maps (Key_Type        => String,
                                                Element_Type    => AWA.Events.Queues.Queue_Ref,
                                                Hash            => Ada.Strings.Hash,
                                                Equivalent_Keys => "=",
                                                "="             => "=");
