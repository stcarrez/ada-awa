# Tips

## UI Presentation Tips

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

