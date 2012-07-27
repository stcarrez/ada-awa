/* Copied from ado-mysql.sql*/
/* File generated automatically by dynamo */
/* Entity types */
CREATE TABLE entity_type (
  /* the entity type identifier */
  `id` INTEGER  AUTO_INCREMENT,
  /* the entity type name (table name) */
  `name` VARCHAR(127) UNIQUE NOT NULL,
  PRIMARY KEY (`id`)
);
/* Sequence generator */
CREATE TABLE sequence (
  /* the sequence name */
  `name` VARCHAR(127) NOT NULL,
  /* the sequence record version */
  `version` int ,
  /* the sequence value */
  `value` BIGINT ,
  /* the sequence block size */
  `block_size` BIGINT ,
  PRIMARY KEY (`name`)
);
INSERT INTO entity_type (name) VALUES
("entity_type")
,("sequence")
;
/* Copied from awa-mysql.sql*/
/* File generated automatically by dynamo */
/* Defines an access key */
CREATE TABLE access_key (
  /* the email id */
  `id` BIGINT NOT NULL,
  /* the access key version. */
  `version` int ,
  /* the access key */
  `access_key` VARCHAR(256) ,
  /* the user identifier */
  `user_id` BIGINT ,
  PRIMARY KEY (`id`)
);
/* Access control */
CREATE TABLE acl (
  /* the unique ACL id */
  `id` BIGINT NOT NULL,
  /* the entity type */
  `entity_type` INTEGER ,
  /* the user identifier */
  `user_id` BIGINT ,
  /* the entity identifier */
  `entity_id` BIGINT ,
  /* whether the entity is writeable */
  `writeable` TINYINT ,
  PRIMARY KEY (`id`)
);
/* A message in the message queue */
CREATE TABLE awa_message (
  /* the message identifier */
  `id` BIGINT NOT NULL,
  /* the message version. */
  `version` int ,
  /* the message priority */
  `priority` INTEGER NOT NULL,
  /* the server which is processing this message */
  `server_id` INTEGER NOT NULL,
  /* the task within the server which is processing this message */
  `task_id` INTEGER NOT NULL,
  /* the message parameters */
  `parameters` VARCHAR(60000) NOT NULL,
  /* the message creation date */
  `create_date` DATETIME NOT NULL,
  /* the message processing date */
  `processing_date` DATETIME ,
  /* the message end processing date */
  `finish_date` DATETIME ,
  /* the message status */
  `status` INTEGER NOT NULL,
  /* the message type */
  `type` INTEGER NOT NULL,
  /* the user who triggered the message */
  `user_id` INTEGER NOT NULL,
  /* the user session who triggered the message */
  `session_id` INTEGER NOT NULL,
  /* the message queue associated with this message */
  `queue_id` INTEGER NOT NULL,
  PRIMARY KEY (`id`)
);
/* A message type */
CREATE TABLE awa_message_type (
  /* the message type identifier */
  `id` INTEGER NOT NULL AUTO_INCREMENT,
  /* the message type name */
  `name` VARCHAR(256) NOT NULL,
  PRIMARY KEY (`id`)
);
/* A message queue */
CREATE TABLE awa_queue (
  /* the queue identifier */
  `id` INTEGER NOT NULL,
  /* the event queue version. */
  `version` int ,
  /* the message queue name */
  `name` VARCHAR(256) NOT NULL,
  /* the server identifier which is associated with this message queue */
  `server_id` INTEGER ,
  PRIMARY KEY (`id`)
);
/* Email address */
CREATE TABLE email (
  /* the email id */
  `id` BIGINT NOT NULL,
  /* the email version. */
  `version` int ,
  /* the email address */
  `email` VARCHAR(256) ,
  /* the user identifier */
  `user_id` BIGINT ,
  PRIMARY KEY (`id`)
);
/* Defines an user session */
CREATE TABLE session (
  /* the user session id */
  `id` BIGINT NOT NULL,
  /* the user session version. */
  `version` int ,
  /* the session start date */
  `start_date` DATETIME NOT NULL,
  /* the session start date */
  `end_date` DATETIME ,
  /* the IP address */
  `ip_address` VARCHAR(255) NOT NULL,
  /* the user identifier */
  `user_id` BIGINT NOT NULL,
  /* the session type */
  `type` INTEGER NOT NULL,
  /* the server instance identifier that created this session */
  `server_id` INTEGER NOT NULL,
  /* the authentication session identifier */
  `auth_id` INTEGER ,
  PRIMARY KEY (`id`)
);
/* Record representing a user */
CREATE TABLE user (
  /* the user id */
  `id` BIGINT NOT NULL,
  /* the user version. */
  `version` int ,
  /* the open id */
  `openid` VARCHAR(256) ,
  /* the user name */
  `name` VARCHAR(256) ,
  /* the user first name */
  `first_name` VARCHAR(256) ,
  /* the user last name */
  `last_name` VARCHAR(256) ,
  /* the user last name */
  `password` VARCHAR(256) ,
  /* the user country */
  `country` VARCHAR(256) ,
  /* the user email address */
  `email_id` INTEGER NOT NULL,
  PRIMARY KEY (`id`)
);
INSERT INTO entity_type (name) VALUES
("access_key")
,("acl")
,("awa_message")
,("awa_message_type")
,("awa_queue")
,("email")
,("session")
,("user")
;
/* Copied from awa-workspaces-mysql.sql*/
/* File generated automatically by dynamo */
/* 
            The workspace allows to group all together the different
            application entities which belong to a user or a set of collaborating users.
            Other entities, for example a Blog, a Wiki space, will link to a
            single workspace.

            The workspace has members which are allowed to access the entities
            that are part of the workspace.  A workspace owner decides which user
            is part of the workspace or not.
         */
CREATE TABLE workspace (
  /* the workspace identifier. */
  `id` INTEGER NOT NULL,
  /* the storage data version. */
  `version` int ,
  /* the workspace creation date. */
  `create_date` DATETIME NOT NULL,
  /* the workspace owner. */
  `owner_fk` BIGINT NOT NULL,
  PRIMARY KEY (`id`)
);
/* 
            The workspace member indicates the users who are part of the workspace.
         */
CREATE TABLE workspace_member (
  /* the member identifier. */
  `id` BIGINT NOT NULL,
  /* the workspace member version. */
  `version` int ,
  /* the member creation date. */
  `create_date` DATETIME NOT NULL,
  /* the workspace member. */
  `user_fk` BIGINT NOT NULL,
  /* the workspace. */
  `workspace_fk` INTEGER NOT NULL,
  PRIMARY KEY (`id`)
);
INSERT INTO entity_type (name) VALUES
("workspace")
,("workspace_member")
;
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
/* Copied from atlas-mysql.sql*/
/* File generated automatically by dynamo */
/* The mblog table. */
CREATE TABLE mblog (
  /* the mblog id */
  `id` BIGINT NOT NULL,
  /* the mblog version */
  `version` int ,
  /* the mblog message */
  `message` VARCHAR(256) ,
  /* the mblog creation date */
  `create_date` DATETIME NOT NULL,
  /* the post author */
  `author_id` INTEGER NOT NULL,
  PRIMARY KEY (`id`)
);
INSERT INTO entity_type (name) VALUES
("mblog")
;
