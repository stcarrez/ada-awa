-----------------------------------------------------------------------
--  awa-users-beans -- ASF Beans for user module
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

with Util.Beans.Basic;
with Util.Beans.Objects;
with Util.Beans.Methods;
with Ada.Strings.Unbounded;

with AWA.Users.Logic;
with AWA.Users.Module;
with AWA.Users.Model;

package AWA.Users.Beans is

   use Ada.Strings.Unbounded;

   type Authenticate_Bean is new Util.Beans.Basic.Bean
     and Util.Beans.Methods.Method_Bean with record
      Module     : AWA.Users.Module.User_Module_Access := null;
      Manager    : AWA.Users.Logic.User_Manager_Access := null;
      Email      : Unbounded_String;
      Password   : Unbounded_String;
      First_Name : Unbounded_String;
      Last_Name  : Unbounded_String;
      Access_Key : Unbounded_String;
   end record;

   type Authenticate_Bean_Access is access all Authenticate_Bean'Class;

   --  Get the value identified by the name.
   overriding
   function Get_Value (From : in Authenticate_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the value identified by the name.
   overriding
   procedure Set_Value (From  : in out Authenticate_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   --  This bean provides some methods that can be used in a Method_Expression
   overriding
   function Get_Method_Bindings (From : in Authenticate_Bean)
                                 return Util.Beans.Methods.Method_Binding_Array_Access;

   procedure Set_Session_Principal (Data : in Authenticate_Bean;
                                    User : in AWA.Users.Model.User_Ref);

   --  Action to register a user
   procedure Register_User (Data    : in out Authenticate_Bean;
                            Outcome : in out Unbounded_String);

   --  Action to verify the user after the registration
   procedure Verify_User (Data    : in out Authenticate_Bean;
                          Outcome : in out Unbounded_String);

   --  Action to trigger the lost password email process.
   procedure Lost_Password (Data    : in out Authenticate_Bean;
                            Outcome : in out Unbounded_String);

   --  Action to validate the reset password key and set a new password.
   procedure Reset_Password (Data    : in out Authenticate_Bean;
                             Outcome : in out Unbounded_String);

   --  Action to authenticate a user.
   procedure Authenticate_User (Data    : in out Authenticate_Bean;
                                Outcome : in out Unbounded_String);

   --  Create an authenticate bean.
   function Create_Authenticate_Bean (Module : in AWA.Users.Module.User_Module_Access)
                                      return Util.Beans.Basic.Readonly_Bean_Access;

end AWA.Users.Beans;
