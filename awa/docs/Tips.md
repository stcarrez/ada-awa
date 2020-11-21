# Tips

## UI Presentation Tips

### Adding a simple page

To add a new presentation page in the application, you can use the
[Dynamo](https://github.com/stcarrez/dynamo) code generator.
The web page is an XHTML file created under the `web` directory.
The page name can contain a directory that will be created if necessary.
The new web page can be configured to use a given layout.
The layout file must exist to be used.  The default layout is `layout`.
You can create a new layout with `add-layout` command.
You can also write your layout by adding an XHTML file in the directory:

```
web/WEB-INF/layouts
```

To create the new web page `web/todo/list.xhtml`, you will use:

```
dynamo add-page todo/list
```

Depending on your application configuration and the URL used by the new
page, you may have to add or modify a `url-policy`.  By default, if
the new URL does not match an existing `url-policy`, the access will
be denied for security reasons.  To allow anonymous users to access
the page, use the following `url-policy`:

```
<url-policy>
  <permission>anonymous</permission>
  <url-pattern>/todo/list.html</url-pattern>
</url-policy>
```

and if you want only logged users, use the following:

```
<url-policy>
  <permission>logged-user</permission>
  <url-pattern>/todo/list.html</url-pattern>
</url-policy>
```

Note: make sure to replace the `.xhtml` extension by `.html`.

### Add Open Graph

Use a combination of `fn:trim`, `fn:substring` and `util:escapeJavaScript`
to create the Open Graph description.  The first two functions will remove
spaces at begining and end of the description and will truncate the string.
The `util:escapeJavaScript` is then necessary to make a value HTML attribute
when the description contains special characters.

```
<meta property="og:description"
 content="#{util:escapeJavaScript(fn:substring(fn:trim(post.description),1,128))}"/>
```

### Formatting dates

A date can easily be formatted by following the Java Server Faces patterns
by using the `f:convertDateTime` converter.  The pattern attribute controls
how the date is formated and it takes into account the locale used to render
the request.  For example:

```
<h:outputText value="#{event.member.payment_date}">
   <f:convertDateTime pattern="%A %d %B %Y"/>
</h:outputText>
```

Sometimes, the date must be formatted within an attribute of some HTML
element.  In that case, the `h:outputText` cannot be used but instead
we can use the `util:formatDate` EL function.  For example:

```
date="#{util:formatDate(post.date,'%Y-%m-%d')}"
```

## Configuration Tips

### Adding a permission on user creation

Sometimes it is useful to add some permission when a user is created.
This can be done programatically but also through some simple configuration.
By using the `on-event` XML definition, it is possible to create a set of
permissions when the `user-create` event is posted, hence during user creation.


The following XML extract from Atlas demonstrator will add several
Wiki permissions to the new user.
```
<on-event name="user-create">
  <action>#{permission.create}</action>
  <property name="entity_type">awa_wiki_space</property>
  <property name="entity_id">1</property>
  <property name="workspace_id">1</property>
  <property name="permission">wiki-page-create,wiki-page-update,wiki-page-delete,wiki-page-view,wiki-space-delete,wiki-space-update</property>
</on-event>
```

### Secure configuration

Setting up the secure configuration is made by using a secure keystore with the `akt` tool.
The secure configuration is stored in the keystore file that `akt` will protect by encrypting each
configuration property with their own encryption key.  In order to setup and use the secure configuration
the following steps are necessary:

* Create the keystore file and protect it with a password or a GPG key,
* Populate the keystore file with the configuration values,
* Launch the server with specific options in order to use and access the keystore file.

The two initial steps are done by using the `akt` tool.

To create the keystore file, one way is to run the `akt` command and give
it the keystore password:

```
mkdir secure
akt create --wallet-key-file=secure/wallet.key -c 100000:300000 \
  secure/config.akt
```

This will generate the `secure/wallet.key` file and setup the keystore file
in `secure/config.akt`.  The password must be given to the server when it is
started.  To avoid that, it is possible to store it in a file and make sure
the file is protected against read and write access.  If the password is
stored in such file, the keystore is created by using:

```
akt create --wallet-key-file=secure/wallet.key \
  --passfile=secure/master.key -c 100000:300000 \
  secure/config.akt
```

Once the keystore is created, the configuration are inserted.  Because the server
command can use only one keystore and have several applications, the configuration
parameter must be prefixed by the application name.  For example, to setup the
`database` configuration for the `atlas` application, you will use the command:

```
akt set --wallet-key-file=secure/wallet.key \
  --passfile=secure/master.key \
  secure/config.akt \
  atlas.database 'mysql://localhost:3306/atlas?user=atlas&password=PiX2ShaimohW6eno
```

To avoid having to specify several configuration parameters when launching the server,
it is good practice to create a server global configuration file and indicate
several parameters that the server will use.  Create a file `secure/config.properties`
that contains:

```
keystore-path=secure/config.akt
keystore-masterkey-path=secure/master.key
keystore-password-path=secure/password.key
```

Then, to start the server we just need to give it the server global
configuration path:

```
bin/atlas-server -c secure/config.properties start
```

Note that in order to use this configuration setup, the directory must have
the `rwx------` rights and files must have the `rw-------` rights.
