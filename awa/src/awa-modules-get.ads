-----------------------------------------------------------------------
--  awa-modules-get -- Get a specific module instance
--  Copyright (C) 2011 Stephane Carrez
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

--  The <b>Get</b> function is an helper that retrieves a given module
--  instance from the current application.
generic
   type Module_Type is new Module with private;
   type Module_Type_Access is access all Module_Type'Class;
   Name : String;
function AWA.Modules.Get return Module_Type_Access;
