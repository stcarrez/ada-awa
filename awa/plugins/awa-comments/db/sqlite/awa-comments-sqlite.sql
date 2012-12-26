/* File generated automatically by dynamo */
/* The Comment table records a user comment associated with a database entity.
The comment can be associated with any other database record. */
CREATE TABLE awa_comments (
  /* the comment publication date */
  `date` DATETIME NOT NULL,
  /* the comment message. */
  `message` VARCHAR(255) NOT NULL,
  /* the entity identifier to which this comment is associated */
  `entity_id` BIGINT ,
  /* the comment identifier */
  `id` BIGINT PRIMARY KEY,
  /*  */
  `version` INTEGER NOT NULL,
  /*  */
  `entity_type` INTEGER NOT NULL,
  /*  */
  `author_id` BIGINT NOT NULL
);
INSERT INTO entity_type (name) VALUES ("awa_comments");
