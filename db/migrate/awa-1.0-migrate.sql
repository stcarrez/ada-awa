ALTER TABLE awa_blog ADD COLUMN `url` VARCHAR(255) BINARY NOT NULL;

ALTER TABLE awa_post CHANGE `text` `text` TEXT NOT NULL;
ALTER TABLE awa_question CHANGE `description` `description` TEXT NOT NULL;
ALTER TABLE awa_answer CHANGE `answer` `answer` TEXT NOT NULL;

ALTER TABLE awa_comments CHANGE `date` `create_date` DATETIME NOT NULL;
ALTER TABLE awa_comments CHANGE `message` `message` TEXT NOT NULL;
ALTER TABLE awa_comments ADD COLUMN `status` INTEGER NOT NULL;

ALTER TABLE awa_post ADD COLUMN `allow_comments` TINYINT NOT NULL;

RENAME TABLE awa_comments TO awa_comment;

ALTER TABLE awa_comment ADD COLUMN `format` INTEGER NOT NULL;

CREATE TABLE awa_changelog (
  /* the changelog identifier. */
  `id` BIGINT NOT NULL,
  /* the changelog date. */
  `date` DATETIME NOT NULL,
  /* the changelog text. */
  `text` VARCHAR(255) BINARY NOT NULL,
  /* the optional entity to which the changelog is associated. */
  `for_entity_id` BIGINT NOT NULL,
  /*  */
  `user_id` BIGINT NOT NULL,
  /*  */
  `entity_type` INTEGER NOT NULL,
  PRIMARY KEY (`id`)
);
INSERT INTO entity_type (name) VALUES
("awa_changelog")
;
