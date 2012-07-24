-----------------------------------------------------------------------
--  awa-storages-beans -- Storage Ada Beans
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

with AWA.Storages.Models;
with AWA.Storages.Modules;

with ASF.Parts;

with Util.Beans.Objects;
with Util.Beans.Basic;
with Util.Beans.Methods;
package AWA.Storages.Beans is

   --  ------------------------------
   --  Upload Bean
   --  ------------------------------
   --  The <b>Upload_Bean</b> allows to upload a file in the storage space.
   type Upload_Bean is new AWA.Storages.Models.Storage_Ref
     and Util.Beans.Basic.Bean and Util.Beans.Methods.Method_Bean with record
      Module  : AWA.Storages.Modules.Storage_Module_Access := null;
   end record;
   type Upload_Bean_Access is access all Upload_Bean'Class;

   --  Get the value identified by the name.
   overriding
   function Get_Value (From : in Upload_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the value identified by the name.
   overriding
   procedure Set_Value (From  : in out Upload_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   --  This bean provides some methods that can be used in a Method_Expression
   overriding
   function Get_Method_Bindings (From : in Upload_Bean)
                                 return Util.Beans.Methods.Method_Binding_Array_Access;

   procedure Save_Part (Bean : in out Upload_Bean;
                        Part : in ASF.Parts.Part'Class);

   --  Upload the file.
   procedure Upload (Bean    : in out Upload_Bean;
                     Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   --  Create the Upload_Bean bean instance.
   function Create_Upload_Bean (Module : in AWA.Storages.Modules.Storage_Module_Access)
                              return Util.Beans.Basic.Readonly_Bean_Access;

end AWA.Storages.Beans;
