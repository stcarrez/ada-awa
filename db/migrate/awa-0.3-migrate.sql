/*
 * Migration from Ada AWA 0.2 to Ada AWA 0.3
 */

ALTER TABLE awa_message ADD COLUMN `count` INTEGER NOT NULL;
ALTER TABLE awa_message ADD COLUMN `entity_id` BIGINT NOT NULL;
ALTER TABLE awa_message ADD COLUMN `entity_type` INTEGER NOT NULL;
ALTER TABLE awa_message ADD COLUMN `message_type_id` BIGINT NOT NULL;

ALTER TABLE awa_message CHANGE `user_id` `user_id` BIGINT;
ALTER TABLE awa_message CHANGE `session_id` `session_id` BIGINT;

RENAME TABLE acl TO awa_acl;
UPDATE entity_type set name = "awa_acl" WHERE name = "acl";
UPDATE sequence set name = "awa_acl" WHERE name = "acl";

RENAME TABLE access_key TO awa_access_key;
UPDATE entity_type set name = "awa_access_key" WHERE name = "access_key";
UPDATE sequence set name = "awa_access_key" WHERE name = "access_key";

ALTER TABLE awa_access_key ADD COLUMN `expire_date` DATETIME NOT NULL;
ALTER TABLE awa_access_key CHANGE `access_key` `access_key` VARCHAR(255) BINARY NOT NULL;

RENAME TABLE email TO awa_email;
UPDATE entity_type set name = "awa_email" WHERE name = "email";
UPDATE sequence set name = "awa_email" WHERE name = "email";

ALTER TABLE awa_email ADD COLUMN `status` TINYINT NOT NULL;
ALTER TABLE awa_email ADD COLUMN `last_error_date` DATETIME NOT NULL;
ALTER TABLE awa_email CHANGE email `email` VARCHAR(255) BINARY NOT NULL;

RENAME TABLE session TO awa_session;
UPDATE entity_type set name = "awa_session" WHERE name = "session";
UPDATE sequence set name = "awa_session" WHERE name = "session";

ALTER TABLE awa_session CHANGE type `stype` TINYINT NOT NULL;

RENAME TABLE user TO awa_user;
UPDATE sequence set name = "awa_user" WHERE name = "user";

ALTER TABLE awa_user CHANGE `first_name` `first_name` VARCHAR(255) BINARY NOT NULL;
ALTER TABLE awa_user CHANGE `last_name` `last_name` VARCHAR(255) BINARY NOT NULL;
ALTER TABLE awa_user CHANGE `password` `password` VARCHAR(255) BINARY NOT NULL;
ALTER TABLE awa_user CHANGE `openid` `open_id` VARCHAR(255) BINARY NOT NULL;
ALTER TABLE awa_user CHANGE `country` `country` VARCHAR(255) BINARY NOT NULL;
ALTER TABLE awa_user CHANGE `name` `name` VARCHAR(255) BINARY NOT NULL;

UPDATE entity_type set name = "awa_user" WHERE name = "user";

/* Copied from awa-comments-mysql.sql*/
/* File generated automatically by dynamo */
/* The Comment table records a user comment associated with a database entity.
The comment can be associated with any other database record. */
CREATE TABLE awa_comments (
  /* the comment publication date */
  `date` DATETIME NOT NULL,
  /* the comment message. */
  `message` VARCHAR(255) BINARY NOT NULL,
  /* the entity identifier to which this comment is associated */
  `entity_id` BIGINT ,
  /* the comment identifier */
  `id` BIGINT NOT NULL,
  /*  */
  `version` INTEGER NOT NULL,
  /*  */
  `entity_type` INTEGER NOT NULL,
  /*  */
  `author_id` BIGINT NOT NULL,
  PRIMARY KEY (`id`)
);
INSERT INTO entity_type (name) VALUES
("awa_comments")
;

RENAME TABLE workspace TO awa_workspace;
UPDATE entity_type set name = "awa_workspace" WHERE name = "workspace";
UPDATE sequence set name = "awa_workspace" WHERE name = "workspace";

ALTER TABLE awa_workspace CHANGE owner_fk  `owner_id` BIGINT NOT NULL;

/*  */
CREATE TABLE awa_workspace_feature (
  /*  */
  `id` BIGINT NOT NULL,
  /*  */
  `limit` INTEGER NOT NULL,
  /*  */
  `workspace_id` BIGINT NOT NULL,
  PRIMARY KEY (`id`)
);

RENAME TABLE workspace_member TO awa_workspace_member;
UPDATE entity_type set name = "awa_workspace_member" WHERE name = "workspace_member";
UPDATE sequence set name = "awa_workspace_member" WHERE name = "workspace_member";

ALTER TABLE awa_workspace_member CHANGE user_fk `member_id` BIGINT NOT NULL;
ALTER TABLE awa_workspace_member CHANGE workspace_fk `workspace_id` BIGINT NOT NULL;

INSERT INTO entity_type (name) VALUES
("awa_workspace_feature")
;

/* Copied from awa-storages-mysql.sql*/
/* File generated automatically by dynamo */
/* The uri member holds the URI if the storage type is URL.

When storage is FILE, the local file path is built by using
the workspace identifier and the storage identifier. */
CREATE TABLE awa_storage (
  /* the storage type which defines where the content is stored */
  `storage` TINYINT NOT NULL,
  /* the storage creation date */
  `create_date` DATETIME NOT NULL,
  /* the file name */
  `name` VARCHAR(255) BINARY NOT NULL,
  /* the file size */
  `file_size` INTEGER NOT NULL,
  /* the mime type */
  `mime_type` VARCHAR(255) BINARY NOT NULL,
  /* the storage URI */
  `uri` VARCHAR(255) BINARY NOT NULL,
  /*  */
  `version` INTEGER NOT NULL,
  /* the storage identifier */
  `id` BIGINT ,
  /*  */
  `original_id` BIGINT ,
  /*  */
  `store_data_id` BIGINT ,
  /*  */
  `owner_id` BIGINT NOT NULL,
  /*  */
  `workspace_id` BIGINT NOT NULL,
  /*  */
  `folder_id` BIGINT ,
  PRIMARY KEY (`id`)
);
/* The storage data is created only if the storage type
is set to DATABASE.  It holds the file content in the blob. */
CREATE TABLE awa_storage_data (
  /* the storage data identifier */
  `id` BIGINT NOT NULL,
  /*  */
  `version` INTEGER NOT NULL,
  /* the storage content */
  `data` LONGBLOB NOT NULL,
  PRIMARY KEY (`id`)
);
/*  */
CREATE TABLE awa_storage_folder (
  /* the storage folder identifier */
  `id` BIGINT NOT NULL,
  /*  */
  `version` INTEGER NOT NULL,
  /* the folder creation date */
  `create_date` DATETIME NOT NULL,
  /*  */
  `name` VARCHAR(255) BINARY NOT NULL,
  /*  */
  `workspace_id` BIGINT NOT NULL,
  /*  */
  `owner_id` BIGINT NOT NULL,
  PRIMARY KEY (`id`)
);
/* The local store record is created when a copy of the data is needed on the local file system.
The creation date refers to the date when the data was copied to the local file system.
The expiration date indicates a date after which the local file can be removed
from the local file system. */
CREATE TABLE awa_store_local (
  /* the local store identifier */
  `id` BIGINT NOT NULL,
  /*  */
  `version` INTEGER NOT NULL,
  /*  */
  `store_version` INTEGER NOT NULL,
  /* the shared flag which indicates whether this local store can be shared by several clients. */
  `shared` TINYINT NOT NULL,
  /* the local store path */
  `path` VARCHAR(255) BINARY NOT NULL,
  /* the local store expiration date */
  `expire_date` DATETIME NOT NULL,
  /* the creation date */
  `create_date` DATETIME NOT NULL,
  /*  */
  `storage_id` BIGINT ,
  PRIMARY KEY (`id`)
);
INSERT INTO entity_type (name) VALUES
("awa_storage")
,("awa_storage_data")
,("awa_storage_folder")
,("awa_store_local")
;
/* Copied from awa-images-mysql.sql*/
/* File generated automatically by dynamo */
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
  /* the image thumbnail height. */
  `thumb_height` INTEGER NOT NULL,
  /* the image thumbnail width. */
  `thumb_width` INTEGER NOT NULL,
  /* the thumbnail image to display the image is an image selector. */
  `thumbnail_id` INTEGER ,
  /* the image storage file. */
  `storage_id` INTEGER NOT NULL,
  PRIMARY KEY (`id`)
);
INSERT INTO entity_type (name) VALUES
("awa_image")
;
/* Copied from awa-votes-mysql.sql*/
/* File generated automatically by dynamo */
/*  */
CREATE TABLE awa_rating (
  /* the rating identifier */
  `id` BIGINT NOT NULL,
  /* the rating taking into account all votes */
  `rating` INTEGER NOT NULL,
  /* the number of votes */
  `vote_count` INTEGER NOT NULL,
  /*  */
  `for_entity_id` BIGINT NOT NULL,
  /*  */
  `for_entity_type` INTEGER NOT NULL,
  PRIMARY KEY (`id`)
);
/* The vote table tracks a vote action by a user on a given database entity.
The primary key is made of the user, the entity id and entity type.
 */
CREATE TABLE awa_vote (
  /*  */
  `rating` INTEGER NOT NULL,
  /*  */
  `id` BIGINT NOT NULL,
  /*  */
  `entity_id` BIGINT NOT NULL,
  /*  */
  `user_id` BIGINT NOT NULL,
  PRIMARY KEY (`id`)
);
INSERT INTO entity_type (name) VALUES
("awa_rating")
,("awa_vote")
;
/* Copied from awa-jobs-mysql.sql*/
/* File generated automatically by dynamo */
/* The job is associated with a dispatching queue. */
CREATE TABLE awa_job (
  /* the job identifier */
  `id` BIGINT NOT NULL,
  /* the job status */
  `status` TINYINT NOT NULL,
  /* the job name */
  `name` VARCHAR(255) BINARY NOT NULL,
  /* the job start date */
  `start_date` DATETIME ,
  /* the job creation date */
  `create_date` DATETIME NOT NULL,
  /* the job finish date */
  `finish_date` DATETIME ,
  /* the job progress indicator */
  `progress` INTEGER NOT NULL,
  /* the job parameters */
  `parameters` VARCHAR(60000) BINARY NOT NULL,
  /* the job result */
  `results` VARCHAR(60000) BINARY NOT NULL,
  /*  */
  `version` INTEGER NOT NULL,
  /* the job priority */
  `priority` INTEGER NOT NULL,
  /*  */
  `user_id` BIGINT ,
  /*  */
  `event_id` BIGINT ,
  /*  */
  `session_id` BIGINT ,
  PRIMARY KEY (`id`)
);
INSERT INTO entity_type (name) VALUES
("awa_job")
;
/* Copied from awa-questions-mysql.sql*/
/* File generated automatically by dynamo */
/* The answer table gives a list of anwsers to the question.
Ranking is updating according to users voting for the anwser.
 */
CREATE TABLE awa_answer (
  /* the answer creation date. */
  `create_date` DATETIME NOT NULL,
  /* the date when the answer was edited. */
  `edit_date` DATETIME ,
  /* the answer text. */
  `answer` VARCHAR(60000) BINARY NOT NULL,
  /* the anwser rank number. */
  `rank` INTEGER NOT NULL,
  /* the answer identifier. */
  `id` BIGINT NOT NULL,
  /*  */
  `version` INTEGER NOT NULL,
  /* the user who wrote the answer. */
  `author_id` BIGINT NOT NULL,
  /*  */
  `question_id` BIGINT NOT NULL,
  PRIMARY KEY (`id`)
);
/* The question table holds a single question asked by a user to the community.
The short description is used to give an overview of the question in long lists
while the description contains the full question text.  The rating is updating
according to users voting for the question. */
CREATE TABLE awa_question (
  /* the date when the question was created. */
  `create_date` DATETIME NOT NULL,
  /* the question title. */
  `title` VARCHAR(255) BINARY NOT NULL,
  /* the full description. */
  `description` VARCHAR(60000) BINARY NOT NULL,
  /* the date when the question was edited. */
  `edit_date` DATETIME ,
  /* Title: Questions and Answers model
Date: 2013-01-07
the question short description. */
  `short_description` VARCHAR(255) BINARY NOT NULL,
  /* the question rating. */
  `rating` INTEGER NOT NULL,
  /* the question identifier. */
  `id` BIGINT NOT NULL,
  /* the optimistic locking version. */
  `version` INTEGER NOT NULL,
  /* the user who asked the question. */
  `author_id` BIGINT NOT NULL,
  /*  */
  `workspace_id` BIGINT NOT NULL,
  /*  */
  `accepted_answer_id` BIGINT ,
  PRIMARY KEY (`id`)
);
INSERT INTO entity_type (name) VALUES
("awa_answer")
,("awa_question")
;

RENAME TABLE blog TO awa_blog;

ALTER TABLE awa_blog ADD COLUMN `update_date` DATETIME NOT NULL;

RENAME TABLE blog_post TO awa_post;
ALTER TABLE awa_post CHANGE COLUMN `title` `title` VARCHAR(255) BINARY NOT NULL;
ALTER TABLE awa_post CHANGE COLUMN `text` `text` VARCHAR(60000) BINARY NOT NULL;

UPDATE entity_type set name = "awa_blog" WHERE name = "blog";
UPDATE sequence set name = "awa_blog" WHERE name = "blog";

UPDATE entity_type set name = "awa_post" WHERE name = "blog_post";
UPDATE sequence set name = "awa_post" WHERE name = "blog_post";
