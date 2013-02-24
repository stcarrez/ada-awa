/* File generated automatically by dynamo */
/* The tag definition. */
CREATE TABLE awa_tag (
  /* the tag identifier */
  `id` BIGINT PRIMARY KEY,
  /* the tag name */
  `name` VARCHAR(255) NOT NULL
);
/*  */
CREATE TABLE awa_tagged_entity (
  /* the tag entity identifier */
  `id` BIGINT PRIMARY KEY,
  /* Title: Tag model
Date: 2013-02-23the database entity to which the tag is associated */
  `for_entity_id` BIGINT NOT NULL,
  /* the entity type */
  `entity_type` INTEGER NOT NULL,
  /*  */
  `tag_id` BIGINT NOT NULL
);
INSERT INTO entity_type (name) VALUES ("awa_tag");
INSERT INTO entity_type (name) VALUES ("awa_tagged_entity");
