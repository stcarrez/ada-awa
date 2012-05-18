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
