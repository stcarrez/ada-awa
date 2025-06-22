/* Migration from AWA 1.1 to AWA 1.2 */

/* File generated automatically by dynamo */
/*  */
CREATE TABLE awa_audit (
  /* the audit identifier */
  `id` BIGINT NOT NULL,
  /* the date when the field was modified. */
  `date` DATETIME NOT NULL,
  /* the old field value. */
  `old_value` VARCHAR(255) BINARY ,
  /* the new field value. */
  `new_value` VARCHAR(255) BINARY ,
  /*  */
  `field` INTEGER NOT NULL,
  /* the database entity identifier to which the audit is associated. */
  `entity_id` BIGINT NOT NULL,
  /* the user session under which the field was modified. */
  `session_id` BIGINT ,
  /* the entity type. */
  `entity_type` INTEGER NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

/* The Audit_Field table describes
the database field being updated. */
CREATE TABLE awa_audit_field (
  /* the audit field identifier. */
  `id` INTEGER NOT NULL AUTO_INCREMENT,
  /* the audit field name. */
  `name` VARCHAR(255) BINARY NOT NULL,
  /* the entity type */
  `entity_type` INTEGER NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

INSERT INTO entity_type (name) VALUES ("awa_audit");
INSERT INTO entity_type (name) VALUES ("awa_audit_field");

INSERT INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM entity_type WHERE name = "awa_wiki_page"), "name");
INSERT INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM entity_type WHERE name = "awa_wiki_page"), "last_version");
INSERT INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM entity_type WHERE name = "awa_wiki_page"), "is_public");
INSERT INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM entity_type WHERE name = "awa_wiki_page"), "title");
INSERT INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM entity_type WHERE name = "awa_wiki_space"), "name");
INSERT INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM entity_type WHERE name = "awa_wiki_space"), "is_public");
INSERT INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM entity_type WHERE name = "awa_wiki_space"), "format");

/* Blog migration: audit support */
INSERT INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM entity_type WHERE name = "awa_blog"), "name");
INSERT INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM entity_type WHERE name = "awa_blog"), "uid");
INSERT INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM entity_type WHERE name = "awa_blog"), "url");
INSERT INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM entity_type WHERE name = "awa_post"), "title");
INSERT INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM entity_type WHERE name = "awa_post"), "uri");
INSERT INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM entity_type WHERE name = "awa_post"), "publish_date");
INSERT INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM entity_type WHERE name = "awa_post"), "status");
INSERT INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM entity_type WHERE name = "awa_post"), "allow_comments");

/* Comment auditing */
INSERT INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM entity_type WHERE name = "awa_comment"), "message");
INSERT INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM entity_type WHERE name = "awa_comment"), "status");
INSERT INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM entity_type WHERE name = "awa_comment"), "format");

/* User auditing */
INSERT INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM entity_type WHERE name = "awa_user"), "first_name");
INSERT INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM entity_type WHERE name = "awa_user"), "last_name");
INSERT INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM entity_type WHERE name = "awa_user"), "country");
INSERT INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM entity_type WHERE name = "awa_user"), "name");

/* Add summary and image_id columns in the blog awa_post table */
ALTER TABLE awa_post ADD COLUMN `summary` VARCHAR(255) BINARY NOT NULL;
ALTER TABLE awa_post ADD COLUMN `image_id` BIGINT;
ALTER TABLE awa_post ADD COLUMN `format` TINYINT NOT NULL DEFAULT 0;

INSERT INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM entity_type WHERE name = "awa_post"), "summary");
INSERT INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM entity_type WHERE name = "awa_post"), "format");

/* Add format and default_image_url in the awa_blog table.  */
ALTER TABLE awa_blog ADD COLUMN `default_image_url` VARCHAR(255) BINARY NOT NULL;
ALTER TABLE awa_blog ADD COLUMN `format` TINYINT NOT NULL DEFAULT 0;

INSERT INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM entity_type WHERE name = "awa_blog"), "format");
INSERT INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM entity_type WHERE name = "awa_blog"), "default_image_url");

