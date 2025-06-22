/* File generated automatically by dynamo */
/*  */
CREATE TABLE IF NOT EXISTS awa_blog (
  /* the blog identifier */
  `id` BIGINT NOT NULL,
  /* the blog name */
  `name` VARCHAR(255) BINARY NOT NULL,
  /* the version */
  `version` INTEGER NOT NULL,
  /* the blog uuid */
  `uid` VARCHAR(255) BINARY NOT NULL,
  /* the blog creation date */
  `create_date` DATETIME NOT NULL,
  /* the date when the blog was updated */
  `update_date` DATETIME NOT NULL,
  /* The blog base URL. */
  `url` VARCHAR(255) BINARY NOT NULL,
  /* the default post format. */
  `format` TINYINT NOT NULL,
  /* the default image URL to be used */
  `default_image_url` VARCHAR(255) BINARY NOT NULL,
  /* the workspace that this blog belongs to */
  `workspace_id` BIGINT NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*  */
CREATE TABLE IF NOT EXISTS awa_post (
  /* the post identifier */
  `id` BIGINT NOT NULL,
  /* the post title */
  `title` VARCHAR(255) BINARY NOT NULL,
  /* the post text content */
  `text` TEXT NOT NULL,
  /* the post creation date */
  `create_date` DATETIME NOT NULL,
  /* the post URI */
  `uri` VARCHAR(255) BINARY NOT NULL,
  /*  */
  `version` INTEGER NOT NULL,
  /* the post publication date */
  `publish_date` DATETIME ,
  /* the post status */
  `status` TINYINT NOT NULL,
  /*  */
  `allow_comments` TINYINT NOT NULL,
  /* the number of times the post was read. */
  `read_count` INTEGER NOT NULL,
  /* the post summary. */
  `summary` VARCHAR(4096) BINARY NOT NULL,
  /* the blog post format. */
  `format` TINYINT NOT NULL,
  /*  */
  `author_id` BIGINT NOT NULL,
  /*  */
  `blog_id` BIGINT NOT NULL,
  /*  */
  `image_id` BIGINT ,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
INSERT IGNORE INTO ado_entity_type (name) VALUES
("awa_blog"), ("awa_post");
INSERT IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = "awa_blog"), "name");
INSERT IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = "awa_blog"), "uid");
INSERT IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = "awa_blog"), "url");
INSERT IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = "awa_blog"), "format");
INSERT IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = "awa_blog"), "default_image_url");
INSERT IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = "awa_post"), "title");
INSERT IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = "awa_post"), "uri");
INSERT IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = "awa_post"), "publish_date");
INSERT IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = "awa_post"), "status");
INSERT IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = "awa_post"), "allow_comments");
INSERT IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = "awa_post"), "summary");
INSERT IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = "awa_post"), "format");
INSERT IGNORE INTO ado_version (name, version) VALUES ("awa-blogs", 1);
