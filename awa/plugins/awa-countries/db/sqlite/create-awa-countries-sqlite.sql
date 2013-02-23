/* Copied from awa-sqlite.sql*/
/* File generated automatically by dynamo */
/*  */
CREATE TABLE awa_message (
  /* the message identifier */
  `id` BIGINT PRIMARY KEY,
  /* the message creation date */
  `create_date` DATETIME NOT NULL,
  /* the message priority */
  `priority` INTEGER NOT NULL,
  /* the message count */
  `count` INTEGER NOT NULL,
  /* the message parameters */
  `parameters` VARCHAR(255) NOT NULL,
  /* the server identifier which processes the message */
  `server_id` INTEGER NOT NULL,
  /* the task identfier on the server which processes the message */
  `task_id` INTEGER NOT NULL,
  /* the message status */
  `status` TINYINT NOT NULL,
  /* the message processing date */
  `processing_date` DATETIME ,
  /*  */
  `version` INTEGER NOT NULL,
  /* the entity identifier to which this event is associated. */
  `entity_id` BIGINT NOT NULL,
  /* the entity type of the entity identifier to which this event is associated. */
  `entity_type` INTEGER NOT NULL,
  /* the date and time when the event was finished to be processed. */
  `finish_date` DATETIME ,
  /*  */
  `queue_id` BIGINT NOT NULL,
  /* the message type */
  `message_type_id` BIGINT NOT NULL,
  /* the optional user who triggered the event message creation */
  `user_id` BIGINT ,
  /* the optional user session that triggered the message creation */
  `session_id` BIGINT 
);
/*  */
CREATE TABLE awa_message_type (
  /*  */
  `id` BIGINT PRIMARY KEY,
  /* the message type name */
  `name` VARCHAR(255) NOT NULL
);
/* The message queue tracks the event messages that must be dispatched by
a given server. */
CREATE TABLE awa_queue (
  /*  */
  `id` BIGINT PRIMARY KEY,
  /*  */
  `server_id` INTEGER NOT NULL,
  /* the message queue name */
  `name` VARCHAR(255) NOT NULL
);
/* The ACL table records permissions which are granted for a user to access a given database entity. */
CREATE TABLE awa_acl (
  /* the ACL identifier */
  `id` BIGINT PRIMARY KEY,
  /* the entity identifier to which the ACL applies */
  `entity_id` BIGINT NOT NULL,
  /* the writeable flag */
  `writeable` TINYINT NOT NULL,
  /*  */
  `user_id` BIGINT NOT NULL,
  /* the entity type concerned by the ACL. */
  `entity_type` INTEGER NOT NULL
);
/*  */
CREATE TABLE awa_access_key (
  /* the secure access key. */
  `access_key` VARCHAR(255) NOT NULL,
  /* the access key expiration date. */
  `expire_date` DATETIME NOT NULL,
  /* the access key identifier. */
  `id` BIGINT PRIMARY KEY,
  /*  */
  `version` INTEGER NOT NULL,
  /*  */
  `user_id` BIGINT NOT NULL
);
/* The Email entity defines the user email addresses.
The user has a primary email address that is obtained
from the registration process (either through a form
submission or through OpenID authentication). */
CREATE TABLE awa_email (
  /* the email address. */
  `email` VARCHAR(255) NOT NULL,
  /* the last mail delivery status (if known). */
  `status` TINYINT NOT NULL,
  /* the date when the last email error was detected. */
  `last_error_date` DATETIME NOT NULL,
  /*  */
  `version` INTEGER NOT NULL,
  /* the email primary key. */
  `id` BIGINT PRIMARY KEY,
  /* the user. */
  `user_id` BIGINT NOT NULL
);
/*  */
CREATE TABLE awa_session (
  /*  */
  `start_date` DATETIME NOT NULL,
  /*  */
  `end_date` DATETIME ,
  /*  */
  `ip_address` VARCHAR(255) NOT NULL,
  /*  */
  `stype` TINYINT NOT NULL,
  /*  */
  `version` INTEGER NOT NULL,
  /*  */
  `server_id` INTEGER NOT NULL,
  /*  */
  `id` BIGINT PRIMARY KEY,
  /*  */
  `auth_id` BIGINT ,
  /*  */
  `user_id` BIGINT NOT NULL
);
/* The User entity represents a user that can access and use the application.
 */
CREATE TABLE awa_user (
  /* the user first name. */
  `first_name` VARCHAR(255) NOT NULL,
  /* the user last name. */
  `last_name` VARCHAR(255) NOT NULL,
  /* the user password hash. */
  `password` VARCHAR(255) NOT NULL,
  /* the user OpenID identifier. */
  `open_id` VARCHAR(255) NOT NULL,
  /* the user country. */
  `country` VARCHAR(255) NOT NULL,
  /* the user display name. */
  `name` VARCHAR(255) NOT NULL,
  /* version number. */
  `version` INTEGER NOT NULL,
  /* the user identifier. */
  `id` BIGINT PRIMARY KEY,
  /*  */
  `email_id` BIGINT NOT NULL
);
INSERT INTO entity_type (name) VALUES ("awa_message");
INSERT INTO entity_type (name) VALUES ("awa_message_type");
INSERT INTO entity_type (name) VALUES ("awa_queue");
INSERT INTO entity_type (name) VALUES ("awa_acl");
INSERT INTO entity_type (name) VALUES ("awa_access_key");
INSERT INTO entity_type (name) VALUES ("awa_email");
INSERT INTO entity_type (name) VALUES ("awa_session");
INSERT INTO entity_type (name) VALUES ("awa_user");
/* Copied from ado-sqlite.sql*/
/* File generated automatically by dynamo */
/* Entity types */
CREATE TABLE entity_type (
  /* the entity type identifier */
  `ID` INTEGER PRIMARY KEY AUTOINCREMENT,
  /* the entity type name (table name) */
  `name` VARCHAR(127) UNIQUE NOT NULL
);
/* Sequence generator */
CREATE TABLE sequence (
  /* the sequence name */
  `name` VARCHAR(127) PRIMARY KEY,
  /* the sequence record version */
  `version` int ,
  /* the sequence value */
  `value` BIGINT ,
  /* the sequence block size */
  `block_size` BIGINT 
);
INSERT INTO entity_type (name) VALUES ("entity_type");
INSERT INTO entity_type (name) VALUES ("sequence");
/* Copied from awa-countries-sqlite.sql*/
/* File generated automatically by dynamo */
/*  */
CREATE TABLE awa_city (
  /* the city identifier */
  `id` INTEGER PRIMARY KEY,
  /* the city name */
  `name` VARCHAR(255) NOT NULL,
  /* the city ZIP code */
  `zip_code` INTEGER NOT NULL,
  /* the city latitude */
  `latitude` INTEGER NOT NULL,
  /* the city longitude */
  `longitude` INTEGER NOT NULL,
  /* the region that this city belongs to */
  `region_id` INTEGER NOT NULL,
  /* the country that this city belongs to */
  `country_id` INTEGER NOT NULL
);
/* The country model is a system data model for the application.
In theory, it never changes. */
CREATE TABLE awa_country (
  /* the country identifier */
  `id` INTEGER PRIMARY KEY,
  /* the country name */
  `name` VARCHAR(255) NOT NULL,
  /* the continent name */
  `continent` VARCHAR(255) NOT NULL,
  /* the currency used in the country */
  `currency` VARCHAR(255) NOT NULL,
  /* the country ISO code */
  `iso_code` VARCHAR(255) NOT NULL,
  /* the country geoname id */
  `geonameid` INTEGER NOT NULL,
  /* the country main language */
  `languages` VARCHAR(255) NOT NULL,
  /* the TLD associated with this country */
  `tld` VARCHAR(3) NOT NULL,
  /* the currency code */
  `currency_code` VARCHAR(3) NOT NULL
);
/* The country neighbor defines what countries
are neigbors with each other */
CREATE TABLE awa_country_neighbor (
  /*  */
  `id` BIGINT PRIMARY KEY,
  /*  */
  `neighbor_of_id` INTEGER NOT NULL,
  /*  */
  `neighbor_id` INTEGER NOT NULL
);
/* Region defines an area within a country. */
CREATE TABLE awa_region (
  /* the region identifier */
  `id` INTEGER PRIMARY KEY,
  /* the region name */
  `name` VARCHAR(255) NOT NULL,
  /* the region geonameid */
  `geonameid` INTEGER NOT NULL,
  /* the country that this region belongs to */
  `country_id` INTEGER NOT NULL
);
INSERT INTO entity_type (name) VALUES ("awa_city");
INSERT INTO entity_type (name) VALUES ("awa_country");
INSERT INTO entity_type (name) VALUES ("awa_country_neighbor");
INSERT INTO entity_type (name) VALUES ("awa_region");
