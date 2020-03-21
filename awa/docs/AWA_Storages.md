# Storages Module
The `storages` module provides a set of storage services allowing
an application to store data files, documents, images in a persistent area.
The persistent store can be on a file system, in the database or provided
by a remote service such as Amazon Simple Storage Service.

## Integration
To be able to use the `storages` module, you will need to add the following
line in your GNAT project file:

```Ada
with "awa_storages";
```

The `Storage_Module` type represents the storage module.  An instance
of the storage module must be declared and registered when the application
is created and initialized.  The storage module is associated with
the storage service which provides and implements the storage management
operations.  An instance of the `Storage_Module` must be declared and
registered in the AWA application.  The module instance can be defined
as follows:

```Ada
with AWA.Storages.Modules;
...
type Application is new AWA.Applications.Application with record
   Storage_Module : aliased AWA.Storages.Modules.Storage_Module;
end record;
```

And registered in the `Initialize_Modules` procedure by using:

```Ada
Register (App    => App.Self.all'Access,
          Name   => AWA.Storages.Modules.NAME,
          Module => App.Storage_Module'Access);
```

## Permissions

| Name           | Entity type  | Description                                                |
|:---------------|:-------------|:-----------------------------------------------------------|
|folder-create|awa_workspace||
|storage-create|awa_workspace||
|storage-delete|awa_workspace||



## Configuration
The `storages` module defines the following configuration parameters:


| Name                      | Description                                                    |
|:--------------------------|:---------------------------------------------------------------|
|storages.storage_root|The path of the directory that contains storage files stored on the local filesystem.|
| |storage|
|storages.tmp_storage_root|The path of the directory that contains temporary storage files on the local filesystem.|
| |tmp|
|storages.database_max_size|The maximum size of documents store in the database storage.|
| |100000|




## Creating a storage
A data in the storage is represented by a `Storage_Ref` instance.
The data itself can be physically stored in a file system (`FILE` mode),
in the database (`DATABASE` mode) or on a remote server (`URL` mode).
To put a file in the storage space, first create the storage object
instance:

```Ada
Data : AWA.Storages.Models.Storage_Ref;
```

Then setup the storage mode that you want.  The storage service uses
this information to save the data in a file, in the database or in
a remote service (in the future).
To save a file in the store, we can use the `Save` operation of the
storage service.
It will read the file and put in in the corresponding persistent store
(the database in this example).

```Ada
Service.Save (Into => Data, Path => Path_To_The_File,
              Storage => AWA.Storages.Models.DATABASE);
```

Upon successful completion, the storage instance `Data` will be allocated
a unique identifier that can be retrieved by `Get_Id` or `Get_Key`.

## Getting the data
Several operations are defined to retrieve the data.  Each of them has been
designed to optimize the retrieval and

  * The data can be retrieved in a local file.
 This mode is useful if an external program must be launched and be able
 to read the file.  If the storage mode of the data is `FILE`, the path
 of the file on the storage file system is used.  For other storage modes,
 the file is saved in a temporary file.  In that case the `Store_Local`
 database table is used to track such locally saved data.

  * The data can be returned as a stream.
 When the application has to read the data, opening a read stream
 connection is the most efficient mechanism.

## Local file
To access the data by using a local file, we must define a local storage
reference:

```Ada
Data : AWA.Storages.Models.Store_Local_Ref;
```

and use the `Load` operation with the storage identifier.  When loading
locally we also indicate whether the file will be read or written.  A file
that is in `READ` mode can be shared by several tasks or processes.
A file that is in `WRITE` mode will have a specific copy for the caller.
An optional expiration parameter indicate when the local file representation
can expire.

```Ada
Service.Load (From => Id, Into => Data, Mode => READ, Expire => ONE_DAY);
```

Once the load operation succeeded, the data is stored on the file system and
the local path is obtained by using the `Get_Path` operation:

```Ada
Path : constant String := Data.Get_Path;
```

## Storage Service
The <tt>Storage_Service</tt> provides the operations to access and use the persisent storage.
It controls the permissions that grant access to the service for users.

Other modules can be notified of storage changes by registering a listener
on the storage module.

## Store Service
The `AWA.Storages.Stores` package defines the interface that a store must implement to
be able to save and retrieve a data content.  The store can be a file system, a database
or a remote store service.

### Database store
The `AWA.Storages.Stores.Databases` store uses the database to save a data content.
The data is saved in a specific table in a database blob column.
The database store uses another store service to temporarily save the data content
in a local file when the application needs a file access to the data.

### File System store
The `AWA.Storages.Stores.Files` store uses the file system to save a data content.
Files are stored in a directory tree whose path is created from the workspace identifier
and the storage identifier.  The layout is such that files belonged to a given workspace
are stored in the same directory sub-tree.

The root directory of the file system store is configured through the
<b>storage_root</b> and <b>tmp_storage_root</b> configuration properties.




## Ada Beans

| Name           | Description                                                               |
|:---------------|:--------------------------------------------------------------------------|
|storageFolder|This bean allows to create a storage folder.|
|uploadFile|This bean allows to upload a new file in the storage space.|
|folderList|This bean gives the list of storage folders in the workspace.|
|storageList|This bean gives the list of storage files associated with a given folder.|
|storageInfo|This bean gives some information about a document and its folder.|




#### AWA.Storages.Models.Storage_Info

The list of documents for a given folder.

| Type     | Ada      | Name       | Description                                             |
|:---------|:---------|:-----------|:--------------------------------------------------------|
||Identifier|id|the storage identifier.|
||String|name|the file name.|
||Date|create_date|the file creation date.|
||String|uri|the file storage URI.|
||AWA.Storages.Models.Storage_Type|storage|the file storage URI.|
||String|mime_type|the file mime type.|
||Integer|file_size|the file size.|
||Boolean|is_public|whether the document is public or not.|
||String|user_name|the user name who uploaded the document.|
||Integer|thumb_width|the image thumbnail width (or 0).|
||Integer|thumb_height|the image thumbnail height (or 0).|
||Identifier|thumbnail_id|the image thumbnail identifier.|





#### AWA.Storages.Models.Folder_Info

The list of folders.

| Type     | Ada      | Name       | Description                                             |
|:---------|:---------|:-----------|:--------------------------------------------------------|
||Identifier|id|the folder identifier.|
||String|name|the folder name.|
||Date|create_date|the blog creation date.|





## Storage Servlet
The <tt>Storage_Servlet</tt> type is the servlet that allows to retrieve the file
content that was uploaded.


## Queries

| Name              | Description                                                           |
|:------------------|:----------------------------------------------------------------------|
|storage-list|Get a list of storage files for a given folder.|



| Name              | Description                                                           |
|:------------------|:----------------------------------------------------------------------|
|storage-folder-list|Get a list of storage folders that a user can see.|



| Name              | Description                                                           |
|:------------------|:----------------------------------------------------------------------|
|storage-get-data|Get the data content of the storage object.|
|storage-get-local|Get the local data storage that can be used to read locally a storage object.|
|storage-get-storage|Get the local data storage that can be used to read locally a storage object.|
|storage-delete-local|Delete the local storage data|



| Name              | Description                                                           |
|:------------------|:----------------------------------------------------------------------|
|storage-info|Get the description of a document.|



## Data model
![](images/awa_storages_model.png)


