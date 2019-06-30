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
