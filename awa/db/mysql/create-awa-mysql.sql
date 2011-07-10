/* Copied from /home/ciceron/work/pam/pam/awa/ado/db/mysql/ado-mysql.sql*/
/* File generated automatically by dynamo */
/* Sequence generator */
create table sequence (
  /* the sequence name */
  `NAME` VARCHAR(256) NOT NULL,
  /* the sequence record version */
  `version` int ,
  /* the sequence value */
  `VALUE` BIGINT ,
  /* the sequence block size */
  `BLOCK_SIZE` BIGINT ,
  primary key (`NAME`)
);
/* Entity types */
create table entity_type (
  /* the entity type identifier */
  `ID` INTEGER  AUTO_INCREMENT,
  /* the entity type name (table name) */
  `NAME` VARCHAR(256) UNIQUE NOT NULL,
  primary key (`ID`)
);
insert into entity_type (name) values
("sequence")
,("entity_type")
;
/* Copied from /home/ciceron/work/pam/pam/awa/awa/db/mysql/awa-mysql.sql*/
/* File generated automatically by dynamo */
/*  */
create table COMMENTS (
  /*  */
  `ID` INTEGER ,
  /*  */
  `VERSION` int ,
  /*  */
  `DATE` TIMESTAMP NOT NULL,
  /*  */
  `MESSAGE` VARCHAR(65000) NOT NULL,
  /*  */
  `ENTITY_ID` INTEGER NOT NULL,
  /*  */
  `USER_FK` INTEGER NOT NULL,
  /*  */
  `ENTITY__TYPE_FK` INTEGER NOT NULL,
  primary key (`ID`)
);
/* Email address */
create table email (
  /* the email id */
  `ID` BIGINT NOT NULL,
  /*  */
  `version` int ,
  /* the email address */
  `EMAIL` VARCHAR(256) ,
  /* the user identifier */
  `USER_ID` BIGINT ,
  primary key (`ID`)
);
/* Record representing a user */
create table user (
  /* the user id */
  `ID` BIGINT NOT NULL,
  /*  */
  `version` int ,
  /* the open id */
  `OPENID` VARCHAR(256) ,
  /* the user name */
  `NAME` VARCHAR(256) ,
  /* the user first name */
  `FIRST_NAME` VARCHAR(256) ,
  /* the user last name */
  `LAST_NAME` VARCHAR(256) ,
  /* the user last name */
  `PASSWORD` VARCHAR(256) ,
  /* the user country */
  `COUNTRY` VARCHAR(256) ,
  /*  */
  `EMAIL_ID` INTEGER NOT NULL,
  primary key (`ID`)
);
/* Defines an access key */
create table access_key (
  /* the email id */
  `ID` BIGINT NOT NULL,
  /*  */
  `version` int ,
  /* the access key */
  `ACCESS_KEY` VARCHAR(256) ,
  /* the user identifier */
  `USER_ID` BIGINT ,
  primary key (`ID`)
);
/* Defines an user session */
create table session (
  /* the user session id */
  `ID` BIGINT NOT NULL,
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
  `TYPE` INTEGER NOT NULL,
  primary key (`ID`)
);
/* Access control */
create table ACL (
  /* the unique ACL id */
  `ID` BIGINT NOT NULL,
  /* the entity type */
  `ENTITY_TYPE` INTEGER ,
  /* the user identifier */
  `USER_ID` BIGINT ,
  /* the entity identifier */
  `ENTITY_ID` BIGINT ,
  /* whether the entity is writeable */
  `WRITEABLE` TINYINT ,
  primary key (`ID`)
);
insert into entity_type (name) values
("COMMENTS")
,("email")
,("user")
,("access_key")
,("session")
,("ACL")
;
/* Copied from ./db/mysql/awa-mysql.sql*/
/* File generated automatically by dynamo */
/*  */
create table COMMENTS (
  /*  */
  `ID` INTEGER ,
  /*  */
  `VERSION` int ,
  /*  */
  `DATE` TIMESTAMP NOT NULL,
  /*  */
  `MESSAGE` VARCHAR(65000) NOT NULL,
  /*  */
  `ENTITY_ID` INTEGER NOT NULL,
  /*  */
  `USER_FK` INTEGER NOT NULL,
  /*  */
  `ENTITY__TYPE_FK` INTEGER NOT NULL,
  primary key (`ID`)
);
/* Email address */
create table email (
  /* the email id */
  `ID` BIGINT NOT NULL,
  /*  */
  `version` int ,
  /* the email address */
  `EMAIL` VARCHAR(256) ,
  /* the user identifier */
  `USER_ID` BIGINT ,
  primary key (`ID`)
);
/* Record representing a user */
create table user (
  /* the user id */
  `ID` BIGINT NOT NULL,
  /*  */
  `version` int ,
  /* the open id */
  `OPENID` VARCHAR(256) ,
  /* the user name */
  `NAME` VARCHAR(256) ,
  /* the user first name */
  `FIRST_NAME` VARCHAR(256) ,
  /* the user last name */
  `LAST_NAME` VARCHAR(256) ,
  /* the user last name */
  `PASSWORD` VARCHAR(256) ,
  /* the user country */
  `COUNTRY` VARCHAR(256) ,
  /*  */
  `EMAIL_ID` INTEGER NOT NULL,
  primary key (`ID`)
);
/* Defines an access key */
create table access_key (
  /* the email id */
  `ID` BIGINT NOT NULL,
  /*  */
  `version` int ,
  /* the access key */
  `ACCESS_KEY` VARCHAR(256) ,
  /* the user identifier */
  `USER_ID` BIGINT ,
  primary key (`ID`)
);
/* Defines an user session */
create table session (
  /* the user session id */
  `ID` BIGINT NOT NULL,
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
  `TYPE` INTEGER NOT NULL,
  primary key (`ID`)
);
/* Access control */
create table ACL (
  /* the unique ACL id */
  `ID` BIGINT NOT NULL,
  /* the entity type */
  `ENTITY_TYPE` INTEGER ,
  /* the user identifier */
  `USER_ID` BIGINT ,
  /* the entity identifier */
  `ENTITY_ID` BIGINT ,
  /* whether the entity is writeable */
  `WRITEABLE` TINYINT ,
  primary key (`ID`)
);
insert into entity_type (name) values
("COMMENTS")
,("email")
,("user")
,("access_key")
,("session")
,("ACL")
;
