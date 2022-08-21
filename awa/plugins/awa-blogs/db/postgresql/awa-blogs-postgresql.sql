/* File generated automatically by dynamo */
/*  */
CREATE TABLE IF NOT EXISTS awa_blog (
  /* the blog identifier */
  "id" BIGINT NOT NULL,
  /* the blog name */
  "name" VARCHAR(255) NOT NULL,
  /* the version */
  "version" INTEGER NOT NULL,
  /* the blog uuid */
  "uid" VARCHAR(255) NOT NULL,
  /* the blog creation date */
  "create_date" TIMESTAMP NOT NULL,
  /* the date when the blog was updated */
  "update_date" TIMESTAMP NOT NULL,
  /* The blog base URL. */
  "url" VARCHAR(255) NOT NULL,
  /* the default post format. */
  "format" SMALLINT NOT NULL,
  /* the default image URL to be used */
  "default_image_url" VARCHAR(255) NOT NULL,
  /* the workspace that this blog belongs to */
  "workspace_id" BIGINT NOT NULL,
  PRIMARY KEY ("id")
);
/*  */
CREATE TABLE IF NOT EXISTS awa_post (
  /* the post identifier */
  "id" BIGINT NOT NULL,
  /* the post title */
  "title" VARCHAR(255) NOT NULL,
  /* the post text content */
  "text" TEXT NOT NULL,
  /* the post creation date */
  "create_date" TIMESTAMP NOT NULL,
  /* the post URI */
  "uri" VARCHAR(255) NOT NULL,
  /*  */
  "version" INTEGER NOT NULL,
  /* the post publication date */
  "publish_date" TIMESTAMP ,
  /* the post status */
  "status" SMALLINT NOT NULL,
  /*  */
  "allow_comments" BOOLEAN NOT NULL,
  /* the number of times the post was read. */
  "read_count" INTEGER NOT NULL,
  /* the post summary. */
  "summary" VARCHAR(4096) NOT NULL,
  /* the blog post format. */
  "format" SMALLINT NOT NULL,
  /*  */
  "author_id" BIGINT NOT NULL,
  /*  */
  "blog_id" BIGINT NOT NULL,
  /*  */
  "image_id" BIGINT ,
  PRIMARY KEY ("id")
);
INSERT INTO ado_entity_type (name) VALUES
('awa_blog'), ('awa_post')
  ON CONFLICT DO NOTHING;
INSERT INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = 'awa_blog'), 'name')
  ON CONFLICT DO NOTHING;
INSERT INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = 'awa_blog'), 'uid')
  ON CONFLICT DO NOTHING;
INSERT INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = 'awa_blog'), 'url')
  ON CONFLICT DO NOTHING;
INSERT INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = 'awa_blog'), 'format')
  ON CONFLICT DO NOTHING;
INSERT INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = 'awa_blog'), 'default_image_url')
  ON CONFLICT DO NOTHING;
INSERT INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = 'awa_post'), 'title')
  ON CONFLICT DO NOTHING;
INSERT INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = 'awa_post'), 'uri')
  ON CONFLICT DO NOTHING;
INSERT INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = 'awa_post'), 'publish_date')
  ON CONFLICT DO NOTHING;
INSERT INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = 'awa_post'), 'status')
  ON CONFLICT DO NOTHING;
INSERT INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = 'awa_post'), 'allow_comments')
  ON CONFLICT DO NOTHING;
INSERT INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = 'awa_post'), 'summary')
  ON CONFLICT DO NOTHING;
INSERT INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = 'awa_post'), 'format')
  ON CONFLICT DO NOTHING;
