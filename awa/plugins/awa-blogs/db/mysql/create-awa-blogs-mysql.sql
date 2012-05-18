/* Copied from awa-blogs-mysql.sql*/
/* File generated automatically by dynamo */
/* Blog  */
CREATE TABLE blog (
  /* the blog identifier */
  `id` INTEGER NOT NULL,
  /* the blob version. */
  `version` int ,
  /* the blog name */
  `name` VARCHAR(256) NOT NULL,
  /* the blog uuid */
  `uid` VARCHAR(256) NOT NULL,
  /* the blog creation date */
  `create_date` DATETIME NOT NULL,
  /* the workspace that this blob belongs to. */
  `workspace_id` INTEGER NOT NULL,
  PRIMARY KEY (`id`)
);
/* Post in a blog */
CREATE TABLE blog_post (
  /* the post identifier */
  `id` BIGINT NOT NULL,
  /* the post version. */
  `version` int ,
  /* the post title */
  `title` VARCHAR(256) NOT NULL,
  /* the uri */
  `uri` VARCHAR(256) ,
  /* the blog text content */
  `text` VARCHAR(60000) ,
  /* the post creation date */
  `create_date` DATETIME NOT NULL,
  /* the post publication date */
  `publish_date` DATETIME ,
  /* the post status */
  `status` INTEGER NOT NULL,
  /* the post author */
  `author_id` INTEGER NOT NULL,
  /* the blog that this post belongs */
  `blog_id` INTEGER NOT NULL,
  PRIMARY KEY (`id`)
);
INSERT INTO entity_type (name) VALUES
("blog")
,("blog_post")
;
