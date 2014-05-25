/* the table that contains the reviews made by users. */
CREATE TABLE atlas_review (
  /* the review identifier */
  `id` BIGINT NOT NULL,
  /*  */
  `version` INTEGER NOT NULL,
  /* the review title. */
  `title` VARCHAR(255) BINARY NOT NULL,
  /* the review description */
  `text` VARCHAR(65535) BINARY NOT NULL,
  /* the review creation date. */
  `create_date` DATETIME NOT NULL,
  /* whether comments are allowed. */
  `allow_comments` INTEGER NOT NULL,
  /* the site, article or application being reviewed. */
  `site` VARCHAR(255) BINARY NOT NULL,
  /*  */
  `reviewer_id` BIGINT NOT NULL,
  PRIMARY KEY (`id`)
);
INSERT INTO entity_type (name) VALUES
("atlas_review")
;
