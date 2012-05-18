/* File generated automatically by dynamo */
/* The Comment table records a user comment associated with a database entity.
                 The comment can be associated with any other database record. */
CREATE TABLE comments (
  /*  */
  `id` INTEGER ,
  /* the comment version. */
  `version` int ,
  /* the comment publication date. */
  `date` TIMESTAMP NOT NULL,
  /* the comment message. */
  `message` VARCHAR(65000) NOT NULL,
  /* the entity identifier to which this comment is associated. */
  `entity_id` INTEGER NOT NULL,
  /* the user who posted this comment */
  `user_fk` INTEGER NOT NULL,
  /* the entity type that correspond to the entity associated with this comment. */
  `entity__type_fk` INTEGER NOT NULL,
  PRIMARY KEY (`id`)
);
INSERT INTO entity_type (name) VALUES
("comments")
;
