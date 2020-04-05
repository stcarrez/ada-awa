-----------------------------------------------------------------------
--  awa-oauth-services -- OAuth Server Side
--  Copyright (C) 2017 Stephane Carrez
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
with Security.OAuth.Servers;
package AWA.OAuth.Services is

   type Auth_Manager is new Security.OAuth.Servers.Auth_Manager with private;
   type Auth_Manager_Access is access all Auth_Manager'Class;

private

   type Auth_Manager is new Security.OAuth.Servers.Auth_Manager with record
      N : Natural;
   end record;

end AWA.OAuth.Services;
