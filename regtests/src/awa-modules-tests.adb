-----------------------------------------------------------------------
--  awa-modules-tests - Unit tests for Modules
--  Copyright (C) 2011, 2012, 2019, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Test_Caller;
with Util.Beans.Basic;
with Util.Beans.Objects;

with Ada.Unchecked_Deallocation;

with EL.Contexts.Default;

with ASF.Applications.Main;
with AWA.Modules.Beans;
with AWA.Services.Contexts;
package body AWA.Modules.Tests is

   use Util.Tests;

   type Test_Module is new AWA.Modules.Module with null record;
   type Test_Module_Access is access all Test_Module'Class;

   type Test_Bean is new Util.Beans.Basic.Bean with record
      Module : Test_Module_Access := null;
      Name   : Util.Beans.Objects.Object;
      Email  : Util.Beans.Objects.Object;
   end record;
   type Test_Bean_Access is access all Test_Bean'Class;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   overriding
   function Get_Value (From : in Test_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the value identified by the name.
   --  If the name cannot be found, the method should raise the No_Value
   --  exception.
   overriding
   procedure Set_Value (From  : in out Test_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   function Create_Form_Bean (Plugin : in Test_Module_Access)
                              return Util.Beans.Basic.Readonly_Bean_Access;

   package Module_Register is new AWA.Modules.Beans (Test_Module, Test_Module_Access);

   package Caller is new Util.Test_Caller (Test, "AWA.Modules");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test ASF.Modules.Register",
                       Test_Create_Module'Access);
      Caller.Add_Test (Suite, "Test ASF.Modules.Find_Module",
                       Test_Find_Module'Access);
      Caller.Add_Test (Suite, "Test ASF.Applications.Main.Register",
                       Test_Create_Application_Module'Access);
      Caller.Add_Test (Suite, "Test ASF.Applications.Main.Create",
                       Test_Create_Application_Module'Access);
      Caller.Add_Test (Suite, "Test ASF.Navigations.Mappers",
                       Test_Module_Navigation'Access);
   end Add_Tests;

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   overriding
   function Get_Value (From : in Test_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = "module" then
         return Util.Beans.Objects.To_Object (From.Module.Get_Name);
      elsif Name = "name" then
         return From.Name;
      elsif Name = "email" then
         return From.Email;
      else
         return Util.Beans.Objects.Null_Object;
      end if;
   end Get_Value;

   --  ------------------------------
   --  Set the value identified by the name.
   --  If the name cannot be found, the method should raise the No_Value
   --  exception.
   --  ------------------------------
   overriding
   procedure Set_Value (From  : in out Test_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if Name = "name" then
         From.Name := Value;
      elsif Name = "email" then
         From.Email := Value;
      end if;
   end Set_Value;

   function Create_Form_Bean (Plugin : in Test_Module_Access)
                              return Util.Beans.Basic.Readonly_Bean_Access is
      Result : constant Test_Bean_Access := new Test_Bean;
   begin
      Result.Module := Plugin;
      return Result.all'Access;
   end Create_Form_Bean;

   --  ------------------------------
   --  Initialize the test application
   --  ------------------------------
   overriding
   procedure Set_Up (T : in out Test) is
      Fact : ASF.Applications.Main.Application_Factory;
      C    : ASF.Applications.Config;
   begin
      Log.Info ("Creating application");
      T.App := new AWA.Applications.Application;
      C.Copy (Util.Tests.Get_Properties);
      C.Set ("app_search_dirs", Util.Tests.Get_Path ("regtests") & ";"
             & C.Get ("app_search_dirs", ""));
      T.App.Initialize (C, Fact);
      T.App.Register ("layoutMsg", "layout");
   end Set_Up;

   --  ------------------------------
   --  Deletes the application object
   --  ------------------------------
   overriding
   procedure Tear_Down (T : in out Test) is
      procedure Free is
         new Ada.Unchecked_Deallocation (Object => AWA.Applications.Application'Class,
                                         Name   => AWA.Applications.Application_Access);
   begin
      Log.Info ("Releasing application");
      Free (T.App);
   end Tear_Down;

   --  ------------------------------
   --  Test creation of module
   --  ------------------------------
   procedure Test_Create_Module (T : in out Test) is
      M   : aliased Test_Module;
      R   : aliased Module_Registry;
      Ctx : AWA.Services.Contexts.Service_Context;
   begin
      Ctx.Set_Context (T.App, null);
      R.Config.Copy (Util.Tests.Get_Properties);
      M.App := T.App.all'Access;
      Register (R'Unchecked_Access, M.App, M'Unchecked_Access, "empty", "uri");

      T.Assert_Equals ("empty", M.Get_Name, "Invalid module name");
      T.Assert_Equals ("uri", M.Get_URI, "Invalid module uri");

      T.Assert (Find_By_Name (R, "empty") = M'Unchecked_Access, "Find_By_Name failed");
      T.Assert (Find_By_URI (R, "uri") = M'Unchecked_Access, "Find_By_URI failed");
   end Test_Create_Module;

   --  ------------------------------
   --  Test looking for module
   --  ------------------------------
   procedure Test_Find_Module (T : in out Test) is
      M   : aliased Test_Module;
      Ctx : AWA.Services.Contexts.Service_Context;
   begin
      Ctx.Set_Context (T.App, null);
      M.App := T.App.all'Access;
      T.App.Register (M'Unchecked_Access, "empty", "uri");

      T.Assert (M.Find_Module ("empty") /= null, "Find_Module should not return a null value");
      T.Assert (M.Find_Module ("toto") = null, "Find_Module should return null");
   end Test_Find_Module;

   --  ------------------------------
   --  Test creation of a module and registration in an application.
   --  ------------------------------
   procedure Test_Create_Application_Module (T : in out Test) is
      use ASF.Beans;

      procedure Check (Name : in String;
                       Kind : in ASF.Beans.Scope_Type);

      procedure Check (Name : in String;
                       Kind : in ASF.Beans.Scope_Type) is
         use type Util.Beans.Basic.Readonly_Bean_Access;

         Value : Util.Beans.Objects.Object;
         Bean  : Util.Beans.Basic.Readonly_Bean_Access;
         Scope : ASF.Beans.Scope_Type;
         Context : EL.Contexts.Default.Default_Context;
      begin
         T.App.Create (Name    => To_Unbounded_String (Name),
                       Context => Context,
                       Result  => Bean,
                       Scope   => Scope);
         T.Assert (Kind = Scope, "Invalid scope for " & Name);
         T.Assert (Bean /= null, "Invalid bean object");
         Value := Util.Beans.Objects.To_Object (Bean);
         T.Assert (not Util.Beans.Objects.Is_Null (Value), "Invalid bean");

         T.Assert_Equals ("test-module",
                          Util.Beans.Objects.To_String (Bean.Get_Value ("module")),
                          "Bean module name");

         --  Special test for the sessionForm bean which is initialized by configuration properties
         if Name = "sessionForm" then
            T.Assert_Equals ("John.Rambo@gmail.com",
                             Util.Beans.Objects.To_String (Bean.Get_Value ("email")),
                             "Session form not initialized");
         end if;
      end Check;

      M   : aliased Test_Module;
      Ctx : AWA.Services.Contexts.Service_Context;
   begin
      Ctx.Set_Context (T.App, null);
      M.App := T.App.all'Access;
      Module_Register.Register (Plugin  => M,
                                Name    => "ASF.Applications.Tests.Form_Bean",
                                Handler => Create_Form_Bean'Access);
      T.App.Register (M'Unchecked_Access, "test-module", "uri");

      T.Assert (M.Find_Module ("test-module") /= null,
                "Find_Module should not return a null value");
      T.Assert (M.Find_Module ("toto") = null, "Find_Module should return null");

      --  Check the 'regtests/config/test-module.xml' managed bean configuration.
      Check ("applicationForm", ASF.Beans.APPLICATION_SCOPE);
      Check ("sessionForm", ASF.Beans.SESSION_SCOPE);
      Check ("requestForm", ASF.Beans.REQUEST_SCOPE);
   end Test_Create_Application_Module;

   --  ------------------------------
   --  Test module and navigation rules
   --  ------------------------------
   procedure Test_Module_Navigation (T : in out Test) is
      M   : aliased Test_Module;
      Ctx : AWA.Services.Contexts.Service_Context;
   begin
      Ctx.Set_Context (T.App, null);
      M.App := T.App.all'Access;
      Module_Register.Register (Plugin  => M,
                                Name    => "ASF.Applications.Tests.Form_Bean",
                                Handler => Create_Form_Bean'Access);
      T.App.Register (M'Unchecked_Access, "test-navigations", "uri");

      T.Assert (M.Find_Module ("test-navigations") /= null,
                "Find_Module should not return a null value");

   end Test_Module_Navigation;

end AWA.Modules.Tests;
