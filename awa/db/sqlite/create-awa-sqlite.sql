/* Copied from /home/ciceron/work/pam/pam/awa/ado/db/sqlite/ado-sqlite.sql*/
/* File generated automatically by dynamo */
/* Sequence generator */
CREATE TABLE sequence (
  /* the sequence name */
  `name` VARCHAR(256) PRIMARY KEY,
  /* the sequence record version */
  `version` int ,
  /* the sequence value */
  `value` BIGINT ,
  /* the sequence block size */
  `block_size` BIGINT 
);
/* Entity types */
CREATE TABLE entity_type (
  /* the entity type identifier */
  `id` INTEGER PRIMARY KEY AUTOINCREMENT,
  /* the entity type name (table name) */
  `name` VARCHAR(256) UNIQUE NOT NULL
);
INSERT INTO entity_type (name) VALUES ("sequence");
INSERT INTO entity_type (name) VALUES ("entity_type");
/* Copied from /home/ciceron/work/pam/pam/awa/awa/db/sqlite/awa-sqlite.sql*/
/* File generated automatically by dynamo */
/* Blog  */
CREATE TABLE blog (
  /* the blog identifier */
  `id` BIGINT PRIMARY KEY,
  /*  */
  `version` int ,
  /* the blog name */
  `name` VARCHAR(256) NOT NULL
);
/* Post in a blog */
CREATE TABLE blog_post (
  /* the post identifier */
  `id` BIGINT PRIMARY KEY,
  /*  */
  `version` int ,
  /* the post title */
  `title` VARCHAR(256) NOT NULL,
  /* the uri */
  `uri` VARCHAR(256) ,
  /* the blog text content */
  `text` VARCHAR(60000) ,
  /* the post creation date */
  `create_date` DATETIME NOT NULL,
  /*  */
  `author_id` INTEGER NOT NULL,
  /*  */
  `blog_id` INTEGER NOT NULL
);
/*  */
CREATE TABLE COMMENTS (
  /*  */
  `id` INTEGER PRIMARY KEY,
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
  `entity__type_fk` INTEGER NOT NULL
);
/* Email address */
CREATE TABLE email (
  /* the email id */
  `id` BIGINT PRIMARY KEY,
  /*  */
  `version` int ,
  /* the email address */
  `email` VARCHAR(256) ,
  /* the user identifier */
  `user_id` BIGINT 
);
/* Record representing a user */
CREATE TABLE user (
  /* the user id */
  `id` BIGINT PRIMARY KEY,
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
  `email_id` INTEGER NOT NULL
);
/* Defines an access key */
CREATE TABLE access_key (
  /* the email id */
  `id` BIGINT PRIMARY KEY,
  /*  */
  `version` int ,
  /* the access key */
  `access_key` VARCHAR(256) ,
  /* the user identifier */
  `user_id` BIGINT 
);
/* Defines an user session */
CREATE TABLE session (
  /* the user session id */
  `ID` BIGINT PRIMARY KEY,
  /*  */
  `version` int ,
  /* the session start date */
  `START_DATE` DATETIME NOT NULL,
  /* the session start date */
  `END_DATE` DATETIME ,
  /* the IP address */
  `IP_ADDRESS` VARCHAR(255) NOT NULL,
  /* the user identifier */
  `USER_ID` BIGINT NOT NULL,
  /* the authentication session identifier */
  `AUTH_ID` BIGINT ,
  /* the session type */
  `TYPE` INTEGER NOT NULL
);
/* Access control */
CREATE TABLE ACL (
  /* the unique ACL id */
  `id` BIGINT PRIMARY KEY,
  /* the entity type */
  `entity_type` INTEGER ,
  /* the user identifier */
  `user_id` BIGINT ,
  /* the entity identifier */
  `entity_id` BIGINT ,
  /* whether the entity is writeable */
  `writeable` TINYINT 
);
INSERT INTO entity_type (name) VALUES ("blog");
INSERT INTO entity_type (name) VALUES ("blog_post");
INSERT INTO entity_type (name) VALUES ("COMMENTS");
INSERT INTO entity_type (name) VALUES ("email");
INSERT INTO entity_type (name) VALUES ("user");
INSERT INTO entity_type (name) VALUES ("access_key");
INSERT INTO entity_type (name) VALUES ("session");
INSERT INTO entity_type (name) VALUES ("ACL");
