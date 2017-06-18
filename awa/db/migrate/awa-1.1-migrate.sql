/* File generated automatically by dynamo */
/*  */
CREATE TABLE awa_wiki_content (
  /* the wiki page content identifier */
  `id` BIGINT NOT NULL,
  /* the wiki content creation date */
  `create_date` DATETIME NOT NULL,
  /* the wiki text content */
  `content` TEXT NOT NULL,
  /* the format type used used by the wiki content */
  `format` TINYINT NOT NULL,
  /* the content comment string */
  `save_comment` VARCHAR(255) BINARY NOT NULL,
  /*  */
  `version` INTEGER NOT NULL,
  /* the wiki page version */
  `page_version` INTEGER NOT NULL,
  /* the wiki page that this Wiki_Content belongs to */
  `page_id` BIGINT NOT NULL,
  /* the page version author */
  `author_id` BIGINT NOT NULL,
  PRIMARY KEY (`id`)
);
/* The wiki page represents a page with its versions.
It refers to the last version which is currently visible. */
CREATE TABLE awa_wiki_page (
  /* the wiki page identifier */
  `id` BIGINT NOT NULL,
  /* the wiki page name */
  `name` VARCHAR(255) BINARY NOT NULL,
  /* the last page version number */
  `last_version` INTEGER NOT NULL,
  /* whether the wiki page is public */
  `is_public` TINYINT NOT NULL,
  /* the page title */
  `title` VARCHAR(255) BINARY NOT NULL,
  /*  */
  `version` INTEGER NOT NULL,
  /* a read counter which indicates how many times the page was read. */
  `read_count` INTEGER NOT NULL,
  /* the wiki page preview. */
  `preview_id` BIGINT ,
  /* the wiki space that this page belongs to */
  `wiki_id` BIGINT NOT NULL,
  /* the current content (or last version) */
  `content_id` BIGINT ,
  PRIMARY KEY (`id`)
);
/* Permission is granted to display a wiki page if there is
an ACL entry between the wiki space and the user. */
CREATE TABLE awa_wiki_space (
  /* the wiki space identifier */
  `id` BIGINT NOT NULL,
  /* the wiki name */
  `name` VARCHAR(255) BINARY NOT NULL,
  /* whether the wiki is public */
  `is_public` TINYINT NOT NULL,
  /*  */
  `version` INTEGER NOT NULL,
  /* the wiki creation date. */
  `create_date` DATETIME NOT NULL,
  /* the left panel side wiki text for every page. */
  `left_side` TEXT NOT NULL,
  /* the right panel wiki text for every page.
 */
  `right_side` TEXT NOT NULL,
  /* the default wiki page format. */
  `format` TINYINT NOT NULL,
  /*  */
  `workspace_id` BIGINT NOT NULL,
  PRIMARY KEY (`id`)
);
INSERT INTO entity_type (name) VALUES
("awa_wiki_content")
,("awa_wiki_page")
,("awa_wiki_space")
;
/* File generated automatically by dynamo */
/*  */
CREATE TABLE awa_counter (
  /* the object associated with the counter. */
  `object_id` BIGINT NOT NULL,
  /* the day associated with the counter. */
  `date` DATE NOT NULL,
  /* the counter value. */
  `counter` INTEGER NOT NULL,
  /* the counter definition identifier. */
  `definition_id` BIGINT NOT NULL,
  PRIMARY KEY (`object_id`, `date`, `definition_id`)
);
/* A counter definition defines what the counter represents. It uniquely identifies
the counter for the Counter table. A counter may be associated with a database
table. In that case, the counter definition has a relation to the corresponding Entity_Type. */
CREATE TABLE awa_counter_definition (
  /* the counter name. */
  `name` VARCHAR(255) BINARY NOT NULL,
  /* the counter unique id. */
  `id` INTEGER NOT NULL,
  /* the optional entity type that identifies the database table. */
  `entity_type` INTEGER ,
  PRIMARY KEY (`id`)
);
/*  */
CREATE TABLE awa_visit (
  /* the entity identifier. */
  `object_id` BIGINT NOT NULL,
  /* the number of times the entity was visited by the user. */
  `counter` INTEGER NOT NULL,
  /* the date and time when the entity was last visited. */
  `date` DATETIME NOT NULL,
  /* the user who visited the entity. */
  `user` BIGINT NOT NULL,
  /* the counter definition identifier. */
  `definition_id` BIGINT NOT NULL,
  PRIMARY KEY (`object_id`, `user`, `definition_id`)
);
INSERT INTO entity_type (name) VALUES
("awa_counter")
,("awa_counter_definition")
,("awa_visit")
;
ALTER TABLE awa_post ADD COLUMN `read_count` INTEGER NOT NULL;
ALTER TABLE awa_image ADD COLUMN `owner_id` BIGINT NOT NULL;
ALTER TABLE awa_image ADD COLUMN `folder_id` BIGINT NOT NULL;
ALTER TABLE awa_image ADD COLUMN `public` TINYINT NOT NULL;
ALTER TABLE awa_image ADD COLUMN `path` VARCHAR(255) BINARY NOT NULL;

ALTER TABLE awa_storage ADD COLUMN `is_public` TINYINT NOT NULL DEFAULT 0;

ALTER TABLE awa_user ADD COLUMN `salt` VARCHAR(255) NOT NULL DEFAULT "";

/*  */
CREATE TABLE awa_invitation (
  /* the invitation identifier. */
  `id` BIGINT NOT NULL,
  /* version optimistic lock. */
  `version` INTEGER NOT NULL,
  /* date when the invitation was created and sent. */
  `create_date` DATETIME NOT NULL,
  /* the email address to which the invitation was sent. */
  `email` VARCHAR(255) BINARY NOT NULL,
  /* the invitation message. */
  `message` text NOT NULL,
  /* the date when the invitation was accepted. */
  `acceptance_date` DATETIME ,
  /* the workspace where the user is invited. */
  `workspace_id` BIGINT NOT NULL,
  /*  */
  `access_key_id` BIGINT ,
  /* the user being invited. */
  `invitee_id` BIGINT ,
  /*  */
  `inviter_id` BIGINT NOT NULL,
  PRIMARY KEY (`id`)
);
INSERT INTO entity_type (name) VALUES ("awa_invitation");

/* The application that is granted access to the database.
 */
CREATE TABLE awa_application (
  /* the application identifier. */
  `id` BIGINT NOT NULL,
  /* the application name. */
  `name` VARCHAR(255) BINARY NOT NULL,
  /* the application secret key. */
  `secret_key` VARCHAR(255) BINARY NOT NULL,
  /* the application public identifier. */
  `client_id` VARCHAR(255) BINARY NOT NULL,
  /* the optimistic lock version. */
  `version` INTEGER NOT NULL,
  /* the application create date. */
  `create_date` DATETIME NOT NULL,
  /* the application update date. */
  `update_date` DATETIME NOT NULL,
  /* the application title displayed in the OAuth login form. */
  `title` VARCHAR(255) BINARY NOT NULL,
  /* the application description. */
  `description` VARCHAR(255) BINARY NOT NULL,
  /* the optional login URL. */
  `app_login_url` VARCHAR(255) BINARY NOT NULL,
  /* the application logo URL. */
  `app_logo_url` VARCHAR(255) BINARY NOT NULL,
  /*  */
  `user_id` BIGINT NOT NULL,
  PRIMARY KEY (`id`)
);
/*  */
CREATE TABLE awa_callback (
  /*  */
  `id` BIGINT NOT NULL,
  /*  */
  `url` VARCHAR(255) BINARY NOT NULL,
  /* the optimistic lock version. */
  `version` INTEGER NOT NULL,
  /*  */
  `application_id` BIGINT NOT NULL,
  PRIMARY KEY (`id`)
);
/* The session is created when the user has granted an access to an application
or when the application has refreshed its access token. */
CREATE TABLE awa_oauth_session (
  /* the session identifier. */
  `id` BIGINT NOT NULL,
  /* the session creation date. */
  `create_date` DATETIME NOT NULL,
  /* a random salt string to access/request token generation. */
  `salt` VARCHAR(255) BINARY NOT NULL,
  /* the expiration date. */
  `expire_date` DATETIME NOT NULL,
  /* the application that is granted access. */
  `application_id` BIGINT NOT NULL,
  /*  */
  `user_id` BIGINT NOT NULL,
  /*  */
  `session_id` BIGINT NOT NULL,
  PRIMARY KEY (`id`)
);
INSERT INTO entity_type (name) VALUES
("awa_application")
,("awa_callback")
,("awa_oauth_session");

ALTER TABLE awa_vote DROP PRIMARY KEY,
  ADD PRIMARY KEY (`entity_id`, `user_id`);
ALTER TABLE awa_vote DROP COLUMN `id`;

/* The permission table lists all the application permissions that are defined.
This is a system table shared by every user and workspace.
The list of permission is fixed and never changes. */
CREATE TABLE awa_permission (
  /* the permission database identifier. */
  `id` BIGINT NOT NULL,
  /* the permission name */
  `name` VARCHAR(255) BINARY NOT NULL,
  PRIMARY KEY (`id`)
);
INSERT INTO entity_type (name) VALUES
("awa_permission");

ALTER TABLE awa_workspace_member ADD COLUMN join_date DATETIME;
ALTER TABLE awa_workspace_member ADD COLUMN role VARCHAR(255);

INSERT INTO awa_workspace_member (id, member_id, workspace_id, create_date, join_date, role)
(SELECT acl.id, acl.user_id, ws.id, ws.create_date, ws.create_date, 'Owner'
   FROM awa_workspace AS ws
   INNER JOIN entity_type AS ent
   INNER JOIN awa_acl AS acl ON acl.entity_id = ws.id AND acl.entity_type = ent.id
   WHERE ent.name = 'awa_workspace');

ALTER TABLE awa_acl ADD COLUMN permission BIGINT NOT NULL;
ALTER TABLE awa_acl ADD COLUMN workspace_id BIGINT NOT NULL;

UPDATE awa_acl
  INNER JOIN entity_type ON awa_acl.entity_type = entity_type.id AND entity_type.name = 'awa_workspace'
  SET awa_acl.workspace_id = awa_acl.entity_id;

UPDATE awa_acl
  INNER JOIN entity_type ON awa_acl.entity_type = entity_type.id AND entity_type.name = 'awa_wiki_space'
  INNER JOIN awa_wiki_space ON awa_acl.entity_id = awa_wiki_space.id
  SET awa_acl.workspace_id = awa_wiki_space.workspace_id;

UPDATE awa_acl
  INNER JOIN entity_type ON awa_acl.entity_type = entity_type.id AND entity_type.name = 'awa_blog'
  INNER JOIN awa_blog ON awa_acl.entity_id = awa_blog.id
  SET awa_acl.workspace_id = awa_blog.workspace_id;

ALTER TABLE awa_invitation ADD COLUMN member_id BIGINT NOT NULL;

ALTER TABLE awa_access_key ADD COLUMN kind TINYINT NOT NULL;


INSERT INTO awa_permission (id, name) VALUES
 (1, 'blog-create'),
 (2, 'blog-delete'),
 (3, 'blog-create-post'),
 (4, 'blog-delete-post'),
 (5, 'blog-add-comment'),
 (6, 'blog-publish-comment'),
 (7, 'blog-delete-comment'),
 (8, 'blog-update-post'),
 (9, 'workspace-create'),
 (10, 'wiki-page-create'),
 (11, 'wiki-page-delete'),
 (12, 'wiki-page-update'),
 (13, 'wiki-page-view'),
 (14, 'wiki-space-create'),
 (15, 'wiki-space-delete'),
 (16, 'wiki-space-update'),
 (17, 'storage-create'),
 (18, 'storage-delete'),
 (19, 'folder-create');

CREATE TEMPORARY TABLE new_acl (
  id integer auto_increment primary key,
  entity_id BIGINT NOT NULL,
  user_id BIGINT NOT NULL,
  workspace_id BIGINT NOT NULL,
  entity_type INTEGER NOT NULL,
  permission INTEGER NOT NULL
);

/* Permissions on the workspace.  */
INSERT INTO new_acl (entity_id, user_id, workspace_id, entity_type, permission)
  SELECT
    acl.entity_id, acl.user_id, acl.workspace_id, acl.entity_type, perm.id
  FROM awa_acl AS acl
  INNER JOIN entity_type AS e ON acl.entity_type = e.id AND acl.permission <= 0
  INNER JOIN awa_permission AS perm ON perm.name
      IN ('blog-create', 'wiki-space-create', 'storage-create', 'folder-create', 'storage-delete')
  WHERE e.name = 'awa_workspace';

/* Permissions on the blog entries */
INSERT INTO new_acl (entity_id, user_id, workspace_id, entity_type, permission)
  SELECT
    acl.entity_id, acl.user_id, acl.workspace_id, acl.entity_type, perm.id
  FROM awa_acl AS acl
  INNER JOIN entity_type AS e ON acl.entity_type = e.id AND acl.permission <= 0
  INNER JOIN awa_permission AS perm ON perm.name
    IN ('blog-update-post', 'blog-delete', 'blog-create-post', 'blog-delete-post',
        'blog-add-comment', 'blog-publish-comment', 'blog-delete-comment')
  WHERE e.name = 'awa_blog';

/* Permissions on the wiki entries */
INSERT INTO new_acl (entity_id, user_id, workspace_id, entity_type, permission)
  SELECT
    acl.entity_id, acl.user_id, acl.workspace_id, acl.entity_type, perm.id
  FROM awa_acl AS acl
  INNER JOIN entity_type AS e ON acl.entity_type = e.id AND acl.permission <= 0
  INNER JOIN awa_permission AS perm ON perm.name
    IN ('wiki-space-update', 'wiki-space-delete', 'wiki-page-create', 'wiki-page-delete',
        'wiki-page-update', 'wiki-page-view')
  WHERE e.name = 'awa_wiki_space';

/* Install the new permissions.  */
DELETE FROM awa_acl;
INSERT INTO awa_acl (id, entity_id, user_id, workspace_id, entity_type, permission)
  SELECT id, entity_id, user_id, workspace_id, entity_type, permission FROM new_acl;

UPDATE sequence
  SET value = GREATEST((SELECT COUNT(*) FROM new_acl), sequence.value)
  WHERE sequence.name = 'awa_acl';

DROP TEMPORARY TABLE new_acl;

