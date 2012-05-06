-----------------------------------------------------------------------
--  atlas-microblog-beans -- Beans for module microblog
--  Copyright (C) 2012 Stephane Carrez
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

with Ada.Strings.Unbounded;

with Util.Beans.Basic;
with Util.Beans.Objects;
with Util.Beans.Methods;
with Atlas.Microblog.Modules;
with Atlas.Microblog.Models;
package Atlas.Microblog.Beans is

   type Microblog_Bean is new Util.Beans.Basic.Bean
     and Util.Beans.Methods.Method_Bean with record
      Module : Atlas.Microblog.Modules.Microblog_Module_Access := null;
      Post   : Atlas.Microblog.Models.Mblog_Ref;
   end record;
   type Microblog_Bean_Access is access all Microblog_Bean'Class;

   --  Get the value identified by the name.
   overriding
   function Get_Value (From : in Microblog_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the value identified by the name.
   overriding
   procedure Set_Value (From  : in out Microblog_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   --  This bean provides some methods that can be used in a Method_Expression
   overriding
   function Get_Method_Bindings (From : in Microblog_Bean)
                                 return Util.Beans.Methods.Method_Binding_Array_Access;

   --  Post the microblog
   procedure Post (Bean    : in out Microblog_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   --  Create the Microblog_Bean bean instance.
   function Create_Microblog_Bean (Module : in Atlas.Microblog.Modules.Microblog_Module_Access)
      return Util.Beans.Basic.Readonly_Bean_Access;

end Atlas.Microblog.Beans;
