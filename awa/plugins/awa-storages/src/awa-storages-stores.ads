-----------------------------------------------------------------------
--  awa-storages-stores -- The storage interface
--  Copyright (C) 2012 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with ADO.Sessions;

with AWA.Storages.Models;

--  == Store Service ==
--  The `AWA.Storages.Stores` package defines the interface that a store must implement to
--  be able to save and retrieve a data content.  The store can be a file system, a database
--  or a remote store service.
--
--  @include awa-storages-stores-databases.ads
--  @include awa-storages-stores-files.ads
package AWA.Storages.Stores is

   --  ------------------------------
   --  Store Service
   --  ------------------------------
   type Store is limited interface;
   type Store_Access is access all Store'Class;

   --  Create a storage
   procedure Create (Storage : in Store;
                     Session : in out ADO.Sessions.Master_Session;
                     From    : in AWA.Storages.Models.Storage_Ref'Class;
                     Into    : in out AWA.Storages.Storage_File) is abstract;

   --  Save the file represented by the `Path` variable into a store and associate that
   --  content with the storage reference represented by `Into`.
   procedure Save (Storage : in Store;
                   Session : in out ADO.Sessions.Master_Session;
                   Into    : in out AWA.Storages.Models.Storage_Ref'Class;
                   Path    : in String) is abstract;

   --  Load the storage item represented by `From` in a file that can be accessed locally.
   procedure Load (Storage : in Store;
                   Session : in out ADO.Sessions.Session'Class;
                   From    : in AWA.Storages.Models.Storage_Ref'Class;
                   Into    : in out AWA.Storages.Storage_File) is abstract;

   --  Delete the content associate with the storage represented by `From`.
   procedure Delete (Storage : in Store;
                     Session : in out ADO.Sessions.Master_Session;
                     From    : in out AWA.Storages.Models.Storage_Ref'Class) is abstract;

end AWA.Storages.Stores;
