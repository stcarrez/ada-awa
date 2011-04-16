/* Email address */
create table email (
  /* the email id */
  `ID` BIGINT,
  /*  */
  `version` int,
  /* the email address */
  `EMAIL` VARCHAR(256),
  /* the user identifier */
  `USER_ID` BIGINT,
  primary key (`id`)
);
/* Record representing a user */
create table user (
  /* the email id */
  `ID` BIGINT,
  /*  */
  `version` int,
  /* the user name */
  `NAME` VARCHAR(256),
  /* the user first name */
  `FIRST_NAME` VARCHAR(256),
  /* the user last name */
  `LAST_NAME` VARCHAR(256),
  /* the user last name */
  `PASSWORD` VARCHAR(256),
  /*  */
  `EMAIL_ID` INTEGER,
  primary key (`id`)
);
/* Defines an access key */
create table access_key (
  /* the email id */
  `ID` BIGINT,
  /*  */
  `version` int,
  /* the access key */
  `ACCESS_KEY` VARCHAR(256),
  /* the user identifier */
  `USER_ID` BIGINT,
  primary key (`id`)
);
/* Defines an user session */
create table session (
  /* the user session id */
  `ID` BIGINT,
  /*  */
  `version` int,
  /* the session start date */
  `START_DATE` DATETIME,
  /* the session start date */
  `END_DATE` DATETIME,
  /* the IP address */
  `IP_ADDRESS` VARCHAR(255),
  /* the user identifier */
  `USER_ID` BIGINT,
  primary key (`id`)
);
/*  */
create table COMMENTS (
  /*  */
  `ID` INTEGER,
  /*  */
  `VERSION` int,
  /*  */
  `DATE` TIMESTAMP,
  /*  */
  `MESSAGE` VARCHAR(256),
  /*  */
  `ENTITY_ID` INTEGER,
  /*  */
  `USER_FK` INTEGER,
  /*  */
  `ENTITY__TYPE_FK` INTEGER,
  primary key (`id`)
);
