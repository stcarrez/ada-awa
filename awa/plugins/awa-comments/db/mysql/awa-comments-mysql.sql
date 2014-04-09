/* File generated automatically by dynamo */
/* The Comment table records a user comment associated with a database entity.
The comment can be associated with any other database record. */
CREATE TABLE awa_comment (
  /* the comment publication date */
  `create_date` DATETIME NOT NULL,
  /* the comment message. */
  `message` TEXT NOT NULL,
  /* the entity identifier to which this comment is associated */
  `entity_id` BIGINT ,
  /* the comment identifier */
  `id` BIGINT NOT NULL,
  /* the optimistic lock version. */
  `version` INTEGER NOT NULL,
  /* the entity type that identifies the table to which the comment is associated. */
  `entity_type` INTEGER NOT NULL,
  /* the comment status to decide whether the comment is visible (published) or not. */
  `status` integer NOT NULL,
  /* the comment format type. */
  `format` integer NOT NULL,
  /*  */
  `author_id` BIGINT NOT NULL,
  PRIMARY KEY (`id`)
);
INSERT INTO entity_type (name) VALUES
("awa_comment")
;
