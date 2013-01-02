/* File generated automatically by dynamo */
/* The workspace controls the features available in the application
for a set of users: the workspace members.  A user could create
several workspaces and be part of several workspaces that other
users have created. */
CREATE TABLE awa_workspace (
  /* the workspace identifier */
  `id` BIGINT PRIMARY KEY,
  /*  */
  `version` INTEGER NOT NULL,
  /*  */
  `create_date` DATETIME NOT NULL,
  /*  */
  `owner_id` BIGINT NOT NULL
);
/*  */
CREATE TABLE awa_workspace_feature (
  /*  */
  `id` BIGINT PRIMARY KEY,
  /*  */
  `limit` INTEGER NOT NULL,
  /*  */
  `workspace_id` BIGINT NOT NULL
);
/* The workspace member indicates the users who
are part of the workspace. */
CREATE TABLE awa_workspace_member (
  /*  */
  `id` BIGINT PRIMARY KEY,
  /*  */
  `member_id` BIGINT NOT NULL,
  /*  */
  `workspace_id` BIGINT NOT NULL
);
INSERT INTO entity_type (name) VALUES ("awa_workspace");
INSERT INTO entity_type (name) VALUES ("awa_workspace_feature");
INSERT INTO entity_type (name) VALUES ("awa_workspace_member");
