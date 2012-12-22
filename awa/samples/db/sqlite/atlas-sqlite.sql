/* File generated automatically by dynamo */
/* The Mblog table holds the message posted by users.
Once posted, the message is not supposed to be changed. */
CREATE TABLE mblog (
  /*  */
  `id` BIGINT PRIMARY KEY,
  /*  */
  `version` INTEGER NOT NULL,
  /* the microblog message */
  `message` VARCHAR(255) NOT NULL,
  /*  */
  `creation_date` DATETIME NOT NULL,
  /* the post author */
  `author_id` BIGINT NOT NULL
);
INSERT INTO entity_type (name) VALUES ("mblog");
