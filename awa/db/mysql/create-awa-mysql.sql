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
  /*  */
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
/* An image that was uploaded by a user in an image folder. */
CREATE TABLE awa_image (
  /* the image identifier. */
  `id` BIGINT NOT NULL,
  /* the image version. */
  `version` int ,
  /* the image width. */
  `width` INTEGER NOT NULL,
  /* the image height. */
  `height` INTEGER NOT NULL,
  /* the task within the server which is processing this message */
  `task_id` INTEGER NOT NULL,
  /* the image name. */
  `name` VARCHAR(255) NOT NULL,
  /* the image type. */
  `mime_type` VARCHAR(255) NOT NULL,
  /* the image path. */
  `path` VARCHAR(255) NOT NULL,
  /* the image creation date. */
  `create_date` DATETIME NOT NULL,
  /* the original image if this image was created by the application. */
  `original_id` INTEGER NOT NULL,
  /* the thumbnail image to display the image is an image selector. */
  `thumbnail_id` INTEGER NOT NULL,
  /* the user who uploaded the image. */
  `user_id` INTEGER NOT NULL,
  /* the image folder where this image is stored. */
  `folder_id` INTEGER NOT NULL,
  /* the image data if the storage type is DATABASE. */
  `image_id` INTEGER ,
  PRIMARY KEY (`id`)
);
/* The image folder contains a set of images that have been uploaded by the user. */
CREATE TABLE awa_image_folder (
  /* the image folder identifier */
  `id` INTEGER NOT NULL AUTO_INCREMENT,
  /* the image folder version. */
  `version` int ,
  /* the image folder name */
  `name` VARCHAR(256) NOT NULL,
  /* the image folder creation date */
  `create_date` DATETIME NOT NULL,
  /* the user who owns this image folder */
  `user_id` INTEGER NOT NULL,
  /* the workspace that this image folder belongs. */
  `workspace_id` INTEGER NOT NULL,
  PRIMARY KEY (`id`)
);
/* A message in the message queue */
CREATE TABLE awa_message (
  /* the message identifier */
  `id` BIGINT NOT NULL,
  /*  */
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
  /*  */
  `version` int ,
  /* the message queue name */
  `name` VARCHAR(256) NOT NULL,
  /* the server identifier which is associated with this message queue */
  `server_id` INTEGER ,
  PRIMARY KEY (`id`)
);
/* The database storage data when the storage type is DATABASE. */
CREATE TABLE awa_storage (
  /* the storage identifier */
  `id` INTEGER NOT NULL,
  /* the storage data version. */
  `version` int ,
  /* the local store creation date */
  `create_date` DATETIME NOT NULL,
  /* the storage type. */
  `storage_type` INTEGER NOT NULL,
  /* the storage specific data */
  `data` VARCHAR(256) NOT NULL,
  /* the storage that this local store refers to. */
  `storage_id` INTEGER NOT NULL,
  /* the workspace that this storage belongs to. */
  `workspace_id` INTEGER NOT NULL,
  PRIMARY KEY (`id`)
);
/* The database storage data when the storage type is DATABASE. */
CREATE TABLE awa_storage_data (
  /* the storage data identifier */
  `id` INTEGER NOT NULL,
  /* the storage data version. */
  `version` int ,
  /* the storage data when the storage type is DATABASE. */
  `data` BLOB NOT NULL,
  PRIMARY KEY (`id`)
);
/* The local store record is created when a copy of the data
is needed on the local file system.  The creation date refers to the date when
the data was copied on the local file system.  The expiration date indicates a
date after which the local file can be removed from the local file system. */
CREATE TABLE awa_store_local (
  /* the local storage identifier */
  `id` INTEGER NOT NULL,
  /* the local storage version. */
  `version` int ,
  /* the local store creation date */
  `create_date` DATETIME NOT NULL,
  /* the local store expiration date */
  `expire_date` DATETIME NOT NULL,
  /* the local store path */
  `name` VARCHAR(256) NOT NULL,
  /* the storage that this local store refers to. */
  `storage_id` INTEGER NOT NULL,
  PRIMARY KEY (`id`)
);
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
/*  */
CREATE TABLE comments (
  /*  */
  `id` INTEGER ,
  /*  */
  `version` int ,
  /*  */
  `date` TIMESTAMP NOT NULL,
  /*  */
  `message` VARCHAR(65000) NOT NULL,
  /*  */
  `entity_id` INTEGER NOT NULL,
  /*  */
  `user_fk` INTEGER NOT NULL,
  /*  */
  `entity__type_fk` INTEGER NOT NULL,
  PRIMARY KEY (`id`)
);
/* Email address */
CREATE TABLE email (
  /* the email id */
  `id` BIGINT NOT NULL,
  /*  */
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
  /*  */
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
  /*  */
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
  /*  */
  `email_id` INTEGER NOT NULL,
  PRIMARY KEY (`id`)
);
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
("access_key")
,("acl")
,("awa_image")
,("awa_image_folder")
,("awa_message")
,("awa_message_type")
,("awa_queue")
,("awa_storage")
,("awa_storage_data")
,("awa_store_local")
,("blog")
,("blog_post")
,("comments")
,("email")
,("session")
,("user")
,("workspace")
,("workspace_member")
;
