### Command Usage

#### SYNOPSIS

_driver_ [-v] [-vv] [-vvv] [-c _config-file_ ] _command_
 [-k _file_ ] [ -d
_dir_ ] [ -p _password_ ] [--password _password_ ] [--passfile
_file_ ] [--passenv _name_ ] [--passfd
_fd_ ] [--passask] [--passcmd
_cmd_ ] [--wallet-key-file
_file_ ]

#### DESCRIPTION

The `AWA.Commands.Drivers` framework integrates the Ada Keystore support
to access some sensitive configuration information such as passwords,
database connection strings, API secret keys.  The use of the Ada Keystore
storage is optional.  It is enabled when the `-k _file_` option is
specified.  When such secure storage is used, a primary password to
unlock the keystore is necessary.  Passwords are retrieved using one
of the following options:

* by reading a file that contains the password,
* by looking at an environment variable,
* by using a command line argument,
* by getting the password through the _ssh-askpass_(1) external command,
* by running an external command,
* by using a GPG private key,
* by asking interactively the user for the password,
* by asking through a network socket for the password.

When the Ada Keystore is used, it is global to all the applications that
are registered in the Web server container.  To allow to differentiate
application specific configuration, each configuration parameter is
prefixed by the application name.

To create and update the keystore file, it is necessary to use the
_akt_(1) tool.  The tool provides many commands for the creation,
insertion, removal and update of content that is stored in the keystore file.

If the keystore file was locked by using GPG, it is not necessary to
specify any specific option to unlock the keystore.  All is needed is the
availability of the _gpg2_(1) command with the private key to unlock the keystore.

#### OPTIONS

The following options are recognized by the command driver:

-V

Prints the
application version.

-v

Enable the verbose mode.

-vv

Enable debugging output.

-vvv

Enable debugging output.

-c _config-file_

Defines the path of the global
server configuration file.

-k file

Specifies the path of the keystore file to open.

-p password

The keystore password is passed within the command line.
Using this method is convenient but is not safe.

--passenv envname

The keystore password is passed within an environment variable with the
given name.  Using this method is considered safer but still provides
some security leaks.

--passfile path

The keystore password is passed within a file that is read.
The file must not be readable or writable by other users or group:
its mode must be r??------.  The directory that contains the file
must also satisfy the not readable by other users or group members,
This method is safer.

--passfd fd

The keystore password is passed within a pipe whose file descriptor
number is given.  The file descriptor is read to obtain the password.
This method is safer.

--passask

The keystore password is retrieved by the running the external tool
_ssh-askpass_(1) which will ask the password through either KDE, Gnome or another
desktop interactive application.
The password is retrieved through a pipe that
the driver sets while launching the command.

--passcmd cmd

The keystore password is retrieved by the running the external command defined in
_cmd_. The command should print the password on its standard output without end of line.
The password is retrieved through a pipe that
the driver sets while launching the command.

--wallet-key-file file

Defines the path of a file which contains the wallet master key file.


#### COMMANDS


##### The start command


_driver_ start [--management-port _PORT_] [--port _PORT_] [--connection _COUNT_]
[--upload _DIR_] [--tcp-no-delay] [--daemon] [--keystore _PATH_]

The `start` command allows to start the server and all applications that are
registered to the web container.  When a keystore is specified, it is first
unlocked by using one of the unlock mechanism (password, GPG, ...).
Then, each application is configured by using its own configuration file
and a subset of the keystore secured configuration.
Once each application is configured, the web server container is started
to listen to the TCP/IP port defined by the `--port=`_PORT_ option.
Applications are then started and they can serve HTTP requests through
the web server container.

At the same time, a management port is created and used exclusively by
the `stop` command to stop the web server container.  The `start` command
will wait on that management port for the `stop` command to be executed.
The management port is configured by the `--management=`_PORT_ option.
The management port is local to the host and cannot be accessed remotely.

When the `--daemon` option is used, the server will first put itself in
the background.  This option is supported only under some Unix systems
like _GNU/Linux_ and _FreeBSD_ and more generally every system where
the `daemon(3)` C-library call is supported.  On other systems, this option
has no effect.

The `--tcp-no-delay` option is supported for recent version of
Ada Web Server and configure the web server to use the `TCP_NO_DELAY`
option of the TCP/IP stack (strongly recommended).

The `--upload=`_DIR_ option indicates to the web server container
the directory that can be used to store temporarily the uploaded files
that the server receives.

The `--connection=`_COUNT_ option controls the maximum number of active
HTTP requests that the server can handle.

##### The setup command

_driver_ setup [--management-port _PORT_] [--port _PORT_] [--connection _COUNT_]
[--upload _DIR_] [--tcp-no-delay] [--daemon] [--keystore _PATH_] _NAME_

The `setup` command is very close to the `start` command but it starts
the [Setup Application](AWA_Setup.md) to configure the application by
using a web browser.


##### The stop command

_driver_ stop [--management-port _PORT_] [--keystore _PATH_]

The `stop` command is used to inform a running web server container to stop.
The management port is used to connect to the web server container and stop it.
The management port is local to the host and cannot be accessed remotely.

The management port is configured with the `--management-port=`_PORT_ option.

##### The list command

_driver_ list [--application _NAME_] [--keystore _PATH_]
[--users] [--jobs] [--sessions] [--tables]

The `list` command is intended to help in looking at the application
database and see some important information.  Because the database is
specific to each application, it may be necessary to indicate the
application name by using the `--application=`_NAME_ option.

The `--tables` option triggers the list of database tables with the
number of entries they contain.

The `--users` option triggers the list of users that are registered in
the application.

The `--sessions` option triggers the list of user connection sessions.

The `--jobs` option triggers the list of jobs that have been created
and scheduled.


