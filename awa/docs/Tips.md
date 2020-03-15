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

