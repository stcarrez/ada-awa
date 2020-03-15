-----------------------------------------------------------------------
--  awa-counters-components -- Counter UI component
--  Copyright (C) 2015, 2018, 2020 Stephane Carrez
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

with ASF.Factory;
with ASF.Contexts.Faces;
with ASF.Components.Html;

--  == HTML components ==
--  The `<awa:counter>` component is an Ada Server Faces component that
--  allows to increment and display easily the counter.  The component
--  works by using the `Counter_Bean` Ada bean object which describes
--  the counter in terms of counter definition, the
--  associated database entity, and the current counter value.
--
--    <awa:counter value="#{wikiPage.counter}"/>
--
--  When the component is included in a page the `Counter_Bean` instance
--  associated with the EL `value` attribute is used to increment the counter.
--  This is similar to calling the `AWA.Counters.Increment` operation
--  from the Ada code.
package AWA.Counters.Components is

   type UICounter is new ASF.Components.Html.UIHtmlComponent with private;

   --  Check if the counter value is hidden.
   function Is_Hidden (UI      : in UICounter;
                       Context : in ASF.Contexts.Faces.Faces_Context'Class) return Boolean;

   --  Render the counter component.  Starts the DL/DD list and write the input
   --  component with the possible associated error message.
   overriding
   procedure Encode_Begin (UI      : in UICounter;
                           Context : in out ASF.Contexts.Faces.Faces_Context'Class);

   --  Get the AWA Counter component factory.
   function Definition return ASF.Factory.Factory_Bindings_Access;

private

   type UICounter is new ASF.Components.Html.UIHtmlComponent with null record;

end AWA.Counters.Components;
