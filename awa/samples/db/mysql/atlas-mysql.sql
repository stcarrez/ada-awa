/* File generated automatically by dynamo */
/* The mblog table. */
CREATE TABLE mblog (
  /* the mblog id */
  `id` BIGINT NOT NULL,
  /* the mblog version */
  `version` int ,
  /* the mblog message */
  `message` VARCHAR(256) ,
  /* the mblog creation date */
  `create_date` DATETIME NOT NULL,
  /* the post author */
  `author_id` INTEGER NOT NULL,
  PRIMARY KEY (`id`)
);
INSERT INTO entity_type (name) VALUES
("mblog")
;
