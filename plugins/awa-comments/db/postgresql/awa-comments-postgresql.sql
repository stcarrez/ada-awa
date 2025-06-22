/* File generated automatically by dynamo */
/* The Comment table records a user comment associated with a database entity.
The comment can be associated with any other database record. */
CREATE TABLE IF NOT EXISTS awa_comment (
  /* the comment publication date */
  "create_date" TIMESTAMP NOT NULL,
  /* the comment message. */
  "message" TEXT NOT NULL,
  /* the entity identifier to which this comment is associated */
  "entity_id" BIGINT NOT NULL,
  /* the comment identifier */
  "id" BIGINT NOT NULL,
  /* the optimistic lock version. */
  "version" INTEGER NOT NULL,
  /* the entity type that identifies the table to which the comment is associated. */
  "entity_type" INTEGER NOT NULL,
  /* the comment status to decide whether the comment is visible (published) or not. */
  "status" INTEGER NOT NULL,
  /* the comment format type. */
  "format" INTEGER NOT NULL,
  /*  */
  "author_id" BIGINT NOT NULL,
  PRIMARY KEY ("id")
);
INSERT INTO ado_entity_type (name) VALUES
('awa_comment')
  ON CONFLICT DO NOTHING;
INSERT INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = 'awa_comment'), 'message')
  ON CONFLICT DO NOTHING;
INSERT INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = 'awa_comment'), 'status')
  ON CONFLICT DO NOTHING;
INSERT INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = 'awa_comment'), 'format')
  ON CONFLICT DO NOTHING;
INSERT INTO ado_version (name, version)
  VALUES ("awa-comments", 1)
  ON CONFLICT DO NOTHING;
