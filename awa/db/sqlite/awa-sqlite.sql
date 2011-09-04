/* File generated automatically by dynamo */
/* Blog  */
create table blog (
  /* the blog identifier */
  `ID` BIGINT PRIMARY KEY,
  /*  */
  `version` int ,
  /* the blog name */
  `NAME` VARCHAR(256) NOT NULL
);
/* Post in a blog */
create table blog_post (
  /* the post identifier */
  `ID` BIGINT PRIMARY KEY,
  /*  */
  `version` int ,
  /* the post title */
  `TITLE` VARCHAR(256) NOT NULL,
  /* the uri */
  `URI` VARCHAR(256) ,
  /* the blog text content */
  `TEXT` VARCHAR(60000) ,
  /* the post creation date */
  `CREATE_DATE` DATETIME NOT NULL,
  /*  */
  `AUTHOR_ID` INTEGER NOT NULL,
  /*  */
  `BLOG_ID` INTEGER NOT NULL
);
/*  */
create table COMMENTS (
  /*  */
  `ID` INTEGER PRIMARY KEY,
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
  `ENTITY__TYPE_FK` INTEGER NOT NULL
);
/* Email address */
create table email (
  /* the email id */
  `ID` BIGINT PRIMARY KEY,
  /*  */
  `version` int ,
  /* the email address */
  `EMAIL` VARCHAR(256) ,
  /* the user identifier */
  `USER_ID` BIGINT 
);
/* Record representing a user */
create table user (
  /* the user id */
  `ID` BIGINT PRIMARY KEY,
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
  `EMAIL_ID` INTEGER NOT NULL
);
/* Defines an access key */
create table access_key (
  /* the email id */
  `ID` BIGINT PRIMARY KEY,
  /*  */
  `version` int ,
  /* the access key */
  `ACCESS_KEY` VARCHAR(256) ,
  /* the user identifier */
  `USER_ID` BIGINT 
);
/* Defines an user session */
create table session (
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
create table ACL (
  /* the unique ACL id */
  `ID` BIGINT PRIMARY KEY,
  /* the entity type */
  `ENTITY_TYPE` INTEGER ,
  /* the user identifier */
  `USER_ID` BIGINT ,
  /* the entity identifier */
  `ENTITY_ID` BIGINT ,
  /* whether the entity is writeable */
  `WRITEABLE` TINYINT 
);
insert into entity_type (name) values ("blog");
insert into entity_type (name) values ("blog_post");
insert into entity_type (name) values ("COMMENTS");
insert into entity_type (name) values ("email");
insert into entity_type (name) values ("user");
insert into entity_type (name) values ("access_key");
insert into entity_type (name) values ("session");
insert into entity_type (name) values ("ACL");
