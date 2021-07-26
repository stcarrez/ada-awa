# Setup Application
The <tt>AWA.Setup</tt> package implements a simple setup application
that allows to configure the database, the Google and Facebook application
identifiers and some other configuration parameters.  It is intended to
help in the installation process of any AWA-based application.

It defines a specific web application that is installed in the web container
for the duration of the setup.  The setup application takes control over all
the web requests during the lifetime of the installation.  As soon as the
installation is finished, the normal application is configured and installed
in the web container and the user is automatically redirected to it.

## Integration
To be able to use the setup application, you will need to add the following
line in your GNAT project file:

```Ada
with "awa_setup";
```

The setup application can be integrated as an AWA command by instantiating
the `AWA.Commands.Setup` generic package.  To integrate the `setup` command,
you will do:

```Ada
with AWA.Commands.Start;
with AWA.Commands.Setup;
...
package Start_Command is new AWA.Commands.Start (Server_Commands);
package Setup_Command is new AWA.Commands.Setup (Start_Command);
```

## Setup Procedure Instantiation
The setup process is managed by the *Configure* generic procedure.
The procedure must be instantiated with the application class type and
the application initialize procedure.

```Ada
 procedure Setup is
    new AWA.Setup.Applications.Configure (MyApp.Application'Class,
                                          MyApp.Application_Access,
                                          MyApp.Initialize);
```

## Setup Operation
The *Setup* instantiated operation must then be called with the web container.
The web container is started first and the *Setup* procedure gets as parameter
the web container, the application instance to configure, the application name
and the application context path.

```Ada
Setup (WS, App, "atlas", MyApp.CONTEXT_PATH)
```

The operation will install the setup application to handle the setup actions.
Through the setup actions, the installer will be able to:

* Configure the database (MySQL or SQLite),

* Configure the Google+ and Facebook OAuth authentication keys,

* Configure the application name,

* Configure the mail parameters to be able to send email.

After the setup and configure is finished, the file <tt>.initialized</tt>
is created in the application directory to indicate the application is
configured.  The next time the *Setup* operation is called, the installation
process will be skipped.

To run again the installation, remove manually the <tt>.initialized</tt> file.

