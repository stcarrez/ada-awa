/* File generated automatically by dynamo */
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
("workspace")
,("workspace_member")
;
