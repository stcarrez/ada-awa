# AWA Core

## Initialization
The AWA application is represented by the `Application` type which should
be extended for the final application to provide the modules and specific
components of the final application.

The initialization of an AWA application is made in several steps
represented by different procedures of the main `Application` type.
The whole initialization is handled by the `Initialize` procedure
which gets a first set of configuration properties and a factory
to build specific component.

The `Initialize` procedure will perform the following steps:

* It uses the factory to allocate the ASF lifecycle handler, the navigation handler, the security manager, the OAuth manager, the exception handlers.
* It calls the `Initialize_Components` procedure to let the application register all the ASF components.  These components must be registered before any configuration file is read.
* It calls the `Initialize_Config` 
* It calls the `Initialize_Servlets` procedure to allow the application to register all the servlet instances used by the application.
* It calls the `Initialize_Filters` procedure to allow the application to register all the servlet filter instances.  The application servlets and filters must be registered before reading the global configuration file.
* It loads the global application configuration by reading the `awa.xml` file.  By reading this configuration, some global configuration is established on the servlets, filters.
* It calls the `Initialize_Modules` procedure so that all the application modules can be registered, configured and initialized.  Each module brings its own component, servlet and filter.  They are configured by their own XML configuration file.
* It loads the module application configuration by reading the XML files described by the `app.config.plugins` configuration.  This last step allows the application to setup and update the configuration of all modules that have been registered.

## Configuration
The following global configuration parameter are defined:


| Name                      | Description                                                    |
|:--------------------------|:---------------------------------------------------------------|
|awa_url_scheme||
| |#{empty app_url_scheme ? 'http://' : app_url_scheme}|
|awa_url_host||
| |#{empty app_url_host ? 'localhost' : app_url_host}|
|awa_url_port||
| |#{empty app_url_port ? ':8080' : app_url_port}|
|app_url_base||
| |#{empty app_url_base ? 'http://localhost:8080' : app_url_base}|
|app_oauth_url_base||
| |http://localhost:8080|
|view.ext||
| |.html|
|web.dir|Defines a list of paths separated by ';' where the XHTML files are searched. The default searches for the 'web' directory in the application search paths.|
| |#{fn:composePath(app_search_dirs,'web')}|
|content-type.default|Defines the default content type for the file servlet.|
| |text/plain|
|ado.queries.load|Controls whether the database query definitions are loaded.|
| |true|
|ado.queries.paths|Defines a list of paths separated by ';' where the database query files are searched. The default searches for the 'db' directory in the application search paths.|
| |#{fn:composePath(app_search_dirs,'db')}|
|bundle.dir|Defines a list of paths separated by ';' where the resource bundle files are searched. The default searches for the 'bundles' directory in the application search paths.|
| |#{fn:composePath(app_search_dirs,'bundles')}|
|app.modules.dir|Defines a list of paths separated by ';' where the module configuration files are searched. The default searches for the 'config' directory in the application search paths.|
| |#{fn:composePath(app_search_dirs,'config')}|




## AWA Modules
A module is a software component that can be integrated in the
web application.  The module can bring a set of service APIs,
some Ada beans and some presentation files.  The AWA framework
allows to configure various parts of a module when it is integrated
in an application.  Some modules are designed to be re-used by
several applications (for example a _mail_ module, a _users_
module, ...).  Other modules could be specific to an application.
An application will be made of several modules, some will be
generic some others specific to the application.

### Registration
The module should have only one instance per application and it must
be registered when the application is initialized.  The module
instance should be added to the application record as follows:

```Ada
type Application is new AWA.Applications.Application with record
   Xxx       : aliased Xxx_Module;
end record;
```

The application record must override the `Initialize_Module` procedure
and it must register the module instance.  This is done as follows:

```Ada
overriding
procedure Initialize_Modules (App : in out Application) is
begin
   Register (App    => App.Self.all'Access,
             Name   => Xxx.Module.NAME,
             URI    => "xxx",
             Module => App.User_Module'Access);
end Initialize_Modules;
```

The module is registered under a unique name.  That name is used
to load the module configuration.

### Configuration
The module is configured by using an XML or a properties file.
The configuration file is used to define:

  * the Ada beans that the module defines and uses,
  * the events that the module wants to receive and the action that must be performed when the event is posted,
  * the permissions that the module needs and how to check them,
  * the navigation rules which are used for the module web interface,
  * the servlet and filter mappings used by the module

The module configuration is located in the *config* directory
and must be the name of the module followed by the file extension
(example: `module-name`.xml or `module-name`.properties).



## AWA Permissions
The *AWA.Permissions* framework defines and controls the permissions used by an application
to verify and grant access to the data and application service.  The framework provides a
set of services and API that helps an application in enforcing its specific permissions.
Permissions are verified by a permission controller which uses the service context to
have information about the user and other context.  The framework allows to use different
kinds of permission controllers.  The `Entity_Controller` is the default permission
controller which uses the database and an XML configuration to verify a permission.

### Declaration
To be used in the application, the first step is to declare the permission.
This is a static definition of the permission that will be used to ask to verify the
permission.  The permission is given a unique name that will be used in configuration files:

```Ada
with Security.Permissions;
...
package ACL_Create_Post is new Security.Permissions.Definition ("blog-create-post");
```

### Checking for a permission
A permission can be checked in Ada as well as in the presentation pages.
This is done by using the `Check` procedure and the permission definition.  This operation
acts as a barrier: it does not return anything but returns normally if the permission is
granted.  If the permission is denied, it raises the `NO_PERMISSION` exception.

Several `Check` operation exists.  Some require no argument and some others need a context
such as some entity identifier to perform the check.

```Ada
with AWA.Permissions;
...
AWA.Permissions.Check (Permission => ACL_Create_Post.Permission,
                       Entity     => Blog_Id);
```

### Configuring a permission
The *AWA.Permissions* framework supports a simple permission model
The application configuration file must provide some information to help in checking the
permission.  The permission name is referenced by the `name` XML entity.  The `entity-type`
refers to the database entity (ie, the table) that the permission concerns.
The `sql` XML entity represents the SQL statement that must be used to verify the permission.

```Ada
<entity-permission>
  <name>blog-create-post</name>
  <entity-type>blog</entity-type>
  <description>Permission to create a new post.</description>
  <sql>
    SELECT acl.id FROM acl
    WHERE acl.entity_type = :entity_type
    AND acl.user_id = :user_id
    AND acl.entity_id = :entity_id
  </sql>
</entity-permission>
```

### Adding a permission
Adding a permission means to create an `ACL` database record that links a given database
entity to the user.  This is done easily with the `Add_Permission` procedure:

```Ada
with AWA.Permissions.Services;
...
AWA.Permissions.Services.Add_Permission (Session => DB,
                                         User    => User,
                                         Entity  => Blog);
```

### Data Model
![](images/awa_permissions_model.png)

### Queries

### Queries

| Name              | Description                                                           |
|:------------------|:----------------------------------------------------------------------|
|check-entity-permission|Get the permission for a user and an entity|
|remove-permission|Delete the permission associated with a user and an object|
|remove-entity-permission|Delete all the permission associated with an object|
|remove-user-permission|Delete all the permission associated with a user|




## AWA Events
The `AWA.Events` package defines an event framework for modules to post
events and have Ada bean methods be invoked when these events are dispatched.
Subscription to events is done through configuration files.  This allows to
configure the modules and integrate them together easily at configuration time.

### Declaration
Modules define the events that they can generate by instantiating the
`Definition` package. This is a static definition of the event.  Each event
is given a unique name.

```Ada
with AWA.Events.Definition;
...
package Event_New_User is new AWA.Events.Definition ("new-user");
```

### Posting an event
The module can post an event to inform other modules or the system that
a particular action occurred.  The module creates the event instance of
type `Module_Event` and populates that event with useful properties for
event receivers.

```Ada
with AWA.Events;
...
Event : AWA.Events.Module_Event;
Event.Set_Event_Kind (Event_New_User.Kind);
Event.Set_Parameter ("email", "harry.potter@hogwarts.org");

```

The module will post the event by using the <b>Send_Event</b> operation.

```Ada
Manager.Send_Event (Event);
```

### Receiving an event
Modules or applications interested by a particular event will configure
the event manager to dispatch the event to an Ada bean event action.
The Ada bean is an object that must implement a procedure that matches
the prototype:

```Ada
type Action_Bean is new Util.Beans.Basic.Readonly_Bean ...;
procedure Action (Bean : in out Action_Bean;
                  Event : in AWA.Events.Module_Event'Class);
```

The Ada bean method and object are registered as other Ada beans.

The configuration file indicates how to bind the Ada bean action and
the event together.  The action is specified using an EL Method Expression
(See [Ada EL](https://github.com/stcarrez/ada-el) or JSR 245).

```Ada
<on-event name="new_user">
    <action>#{ada_bean.action}</action>
</on-event>
```

### Event queues and dispatchers
The `AWA.Events` framework posts events on queues and it uses a dispatcher
to process them.  There are two kinds of dispatchers:

  * Synchronous dispatcher process the event when it is posted. The task which posts the event invokes the Ada bean action. In this dispatching mode, there is no event queue. If the action method raises an exception, it will however be blocked.

  * Asynchronous dispatcher are executed by dedicated tasks.  The event is put in an event queue.  A dispatcher task processes the event and invokes the action method at a later time.

When the event is queued, there are two types of event queues:

  * A Fifo memory queue manages the event and dispatches them in FIFO order. If the application is stopped, the events present in the Fifo queue are lost.

  * A persistent event queue manages the event in a similar way as the FIFO queue but saves them in the database.  If the application is stopped, events that have not yet been processed will be dispatched when the application is started again.

### Data Model
![](images/awa_events_model.png)



### Beans

| Name           | Description                                                               |
|:---------------|:--------------------------------------------------------------------------|
|jquery||

### Configuration

| Name                      | Description                                                    |
|:--------------------------|:---------------------------------------------------------------|
|awa_url_scheme||
| |#{empty app_url_scheme ? 'http://' : app_url_scheme}|
|awa_url_host||
| |#{empty app_url_host ? 'localhost' : app_url_host}|
|awa_url_port||
| |#{empty app_url_port ? ':8080' : app_url_port}|
|app_url_base||
| |#{empty app_url_base ? 'http://localhost:8080' : app_url_base}|
|app_oauth_url_base||
| |http://localhost:8080|
|view.ext||
| |.html|
|web.dir|Defines a list of paths separated by ';' where the XHTML files are searched. The default searches for the 'web' directory in the application search paths.|
| |#{fn:composePath(app_search_dirs,'web')}|
|content-type.default|Defines the default content type for the file servlet.|
| |text/plain|
|ado.queries.load|Controls whether the database query definitions are loaded.|
| |true|
|ado.queries.paths|Defines a list of paths separated by ';' where the database query files are searched. The default searches for the 'db' directory in the application search paths.|
| |#{fn:composePath(app_search_dirs,'db')}|
|bundle.dir|Defines a list of paths separated by ';' where the resource bundle files are searched. The default searches for the 'bundles' directory in the application search paths.|
| |#{fn:composePath(app_search_dirs,'bundles')}|
|app.modules.dir|Defines a list of paths separated by ';' where the module configuration files are searched. The default searches for the 'config' directory in the application search paths.|
| |#{fn:composePath(app_search_dirs,'config')}|



