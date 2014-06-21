/* File generated automatically by dynamo */
/*  */
CREATE TABLE Changelog (
  /* the changelog identifier. */
  `id` BIGINT PRIMARY KEY,
  /* the changelog date. */
  `date` DATETIME NOT NULL,
  /* the changelog text. */
  `text` VARCHAR(255) NOT NULL,
  /* the optional entity to which the changelog is associated. */
  `for_entity_id` BIGINT NOT NULL,
  /*  */
  `user_id` BIGINT NOT NULL,
  /*  */
  `entity_type` INTEGER NOT NULL
);
INSERT INTO entity_type (name) VALUES ("Changelog");
