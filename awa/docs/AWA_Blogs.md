# Blogs Module
The *blogs* plugin is a small blog application which allows users to publish articles.
A user may own several blogs, each blog having a name and its own base URI.  Within a blog,
the user may write articles and publish them.  Once published, the articles are visible to
anonymous users.

The *blogs* plugin uses the [[|AWA_Tags]]] and [[|AWA_Comments]]] modules to allow to associate
tags to a post and allow users to comment on the articles.

## Integration
The `Blog_Module` manages the creation, update, removal of blog posts in an application.
It provides operations that are used by the blog beans or other services to create and update
posts.  An instance of the `Blog_Module` must be declared and registered in the
AWA application.  The module instance can be defined as follows:

```Ada
type Application is new AWA.Applications.Application with record
   Blog_Module : aliased AWA.Blogs.Modules.Blog_Module;
end record;
```

And registered in the `Initialize_Modules` procedure by using:

```Ada
Register (App    => App.Self.all'Access,
          Name   => AWA.Blogs.Modules.NAME,
          URI    => "blogs",
          Module => App.Blog_Module'Access);
```


## Ada Beans
Several bean types are provided to represent and manage the blogs and their posts.
The blog module registers the bean constructors when it is initialized.
To use them, one must declare a bean definition in the application XML configuration.




#### AWA.Blogs.Models.Admin_Post_Info

The Admin_Post_Info describes a post in the administration interface.

| Type     | Ada      | Name       | Description                                             |
|:---------|:---------|:-----------|:--------------------------------------------------------|
||Identifier|id|the post identifier.|
||String|title|the post title.|
||String|uri|the post uri.|
||Date|date|the post publish date.|
||AWA.Blogs.Models.Post_Status_Type|status|the post status.|
||Natural|read_count|the number of times the post was read.|
||String|username|the user name.|
||Natural|comment_count|the number of comments for this post.|





#### AWA.Blogs.Models.Post_Info

The Post_Info describes a post to be displayed in the blog page

| Type     | Ada      | Name       | Description                                             |
|:---------|:---------|:-----------|:--------------------------------------------------------|
||Identifier|id|the post identifier.|
||String|title|the post title.|
||String|uri|the post uri.|
||Date|date|the post publish date.|
||String|username|the user name.|
||String|text|the post text.|
||Boolean|allow_comments|the post allows to add comments.|
||Natural|comment_count|the number of comments for this post.|





#### AWA.Blogs.Models.Comment_Info

The comment information.

| Type     | Ada      | Name       | Description                                             |
|:---------|:---------|:-----------|:--------------------------------------------------------|
||Identifier|id|the comment identifier.|
||Identifier|post_id|the post identifier.|
||String|title|the post title.|
||String|author|the comment author's name.|
||String|email|the comment author's email.|
||Date|date|the comment date.|
||AWA.Comments.Models.Status_Type|status|the comment status.|





#### AWA.Blogs.Models.Blog_Info

The list of blogs.

| Type     | Ada      | Name       | Description                                             |
|:---------|:---------|:-----------|:--------------------------------------------------------|
||Identifier|id|the blog identifier.|
||String|title|the blog title.|
||String|uid|the blog uuid.|
||Date|create_date|the blog creation date.|
||Integer|post_count|the number of posts published.|





## Queries
| Name              | Description                                                           |
|:------------------|:----------------------------------------------------------------------|
|blog-admin-post-list|Get the list of blog posts|
|blog-admin-post-list-date|Get the list of blog posts|


| Name              | Description                                                           |
|:------------------|:----------------------------------------------------------------------|
|blog-post-list|Get the list of public visible posts|
|blog-post-tag-list|Get the list of public visible posts filtered by a tag|


| Name              | Description                                                           |
|:------------------|:----------------------------------------------------------------------|
|comment-list|Get the list of comments associated with given database entity|


| Name              | Description                                                           |
|:------------------|:----------------------------------------------------------------------|
|blog-list|Get the list of blogs that the current user can update|


| Name              | Description                                                           |
|:------------------|:----------------------------------------------------------------------|
|blog-tag-cloud|Get the list of tags associated with all the database entities of a given type|



## Model
![](images/awa_blogs_model.png)


