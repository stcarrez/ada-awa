/* Copied from /home/ciceron/work/pam/pam/awa/ado/db/sqlite/ado-sqlite.sql*/
/* File generated automatically by dynamo */
/* Sequence generator */
create table sequence (
  /* the sequence name */
  `NAME` VARCHAR(256) PRIMARY KEY,
  /* the sequence record version */
  `version` int NOT NULL,
  /* the sequence value */
  `VALUE` BIGINT NOT NULL,
  /* the sequence block size */
  `BLOCK_SIZE` BIGINT NOT NULL
);
/* Entity types */
create table entity_type (
  /* the entity type identifier */
  `ID` INTEGER PRIMARY KEY AUTOINCREMENT,
  /* the entity type name (table name) */
  `NAME` VARCHAR(256) UNIQUE NOT NULL
);
insert into entity_type (name) values ("sequence");
insert into entity_type (name) values ("entity_type");
/* Copied from /home/ciceron/work/pam/pam/awa/awa/db/sqlite/awa-sqlite.sql*/
/* File generated automatically by dynamo */
/*  */
create table COMMENTS (
  /*  */
  `ID` INTEGER PRIMARY KEY,
  /*  */
  `VERSION` int NOT NULL,
  /*  */
  `DATE` TIMESTAMP NOT NULL,
  /*  */
  `MESSAGE` VARCHAR(65000) NOT NULL,
  /*  */
  `ENTITY_ID` INTEGER NOT NULL,
  /*  */
  `USER_FK` INTEGER NOT NULL,
  /*  */
  `ENTITY__TYPE_FK` INTEGER NOT NULL
);
/* Email address */
create table email (
  /* the email id */
  `ID` BIGINT PRIMARY KEY,
  /*  */
  `version` int NOT NULL,
  /* the email address */
  `EMAIL` VARCHAR(256) NOT NULL,
  /* the user identifier */
  `USER_ID` BIGINT NOT NULL
);
/* Record representing a user */
create table user (
  /* the user id */
  `ID` BIGINT PRIMARY KEY,
  /*  */
  `version` int NOT NULL,
  /* the open id */
  `OPENID` VARCHAR(256) NOT NULL,
  /* the user name */
  `NAME` VARCHAR(256) NOT NULL,
  /* the user first name */
  `FIRST_NAME` VARCHAR(256) NOT NULL,
  /* the user last name */
  `LAST_NAME` VARCHAR(256) NOT NULL,
  /* the user last name */
  `PASSWORD` VARCHAR(256) NOT NULL,
  /* the user country */
  `COUNTRY` VARCHAR(256) NOT NULL,
  /*  */
  `EMAIL_ID` INTEGER NOT NULL
);
/* Defines an access key */
create table access_key (
  /* the email id */
  `ID` BIGINT PRIMARY KEY,
  /*  */
  `version` int NOT NULL,
  /* the access key */
  `ACCESS_KEY` VARCHAR(256) NOT NULL,
  /* the user identifier */
  `USER_ID` BIGINT NOT NULL
);
/* Defines an user session */
create table session (
  /* the user session id */
  `ID` BIGINT PRIMARY KEY,
  /*  */
  `version` int NOT NULL,
  /* the session start date */
  `START_DATE` DATETIME NOT NULL,
  /* the session start date */
  `END_DATE` DATETIME NOT NULL,
  /* the IP address */
  `IP_ADDRESS` VARCHAR(255) NOT NULL,
  /* the user identifier */
  `USER_ID` BIGINT NOT NULL,
  /* the authentication session identifier */
  `AUTH_ID` BIGINT NOT NULL,
  /* the session type */
  `TYPE` INTEGER NOT NULL
);
/* Access control */
create table ACL (
  /* the unique ACL id */
  `ID` BIGINT PRIMARY KEY,
  /* the entity type */
  `ENTITY_TYPE` INTEGER NOT NULL,
  /* the user identifier */
  `USER_ID` BIGINT NOT NULL,
  /* the entity identifier */
  `ENTITY_ID` BIGINT NOT NULL,
  /* whether the entity is writeable */
  `WRITEABLE` TINYINT NOT NULL
);
insert into entity_type (name) values ("COMMENTS");
insert into entity_type (name) values ("email");
insert into entity_type (name) values ("user");
insert into entity_type (name) values ("access_key");
insert into entity_type (name) values ("session");
insert into entity_type (name) values ("ACL");
