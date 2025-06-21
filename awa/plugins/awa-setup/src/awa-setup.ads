-----------------------------------------------------------------------
--  awa-setup -- Setup and installation
--  Copyright (C) 2016, 2017, 2018, 2020 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

--  = Setup Application =
--  The <tt>AWA.Setup</tt> package implements a simple setup application
--  that allows to configure the database, the Google and Facebook application
--  identifiers and some other configuration parameters.  It is intended to
--  help in the installation process of any AWA-based application.
--
--  It defines a specific web application that is installed in the web container
--  for the duration of the setup.  The setup application takes control over all
--  the web requests during the lifetime of the installation.  As soon as the
--  installation is finished, the normal application is configured and installed
--  in the web container and the user is automatically redirected to it.
--
--  == Integration ==
--  To be able to use the setup application, you will need to add the following
--  line in your GNAT project file:
--
--    with "awa_setup";
--
--  The setup application can be integrated as an AWA command by instantiating
--  the `AWA.Commands.Setup` generic package.  To integrate the `setup` command,
--  you will do:
--
--    with AWA.Commands.Start;
--    with AWA.Commands.Setup;
--    ...
--    package Start_Command is new AWA.Commands.Start (Server_Commands);
--    package Setup_Command is new AWA.Commands.Setup (Start_Command);
--
--  @include awa-setup-applications.ads
package AWA.Setup is

   pragma Pure;

end AWA.Setup;
