/* File generated automatically by dynamo */
/* The vote table tracks a vote action by a user on a given database entity.
The primary key is made of the user, the entity id and entity type.
 */
CREATE TABLE awa_vote (
  /*  */
  `for_entity_id` BIGINT NOT NULL,
  /*  */
  `rating` INTEGER NOT NULL,
  /*  */
  `user_id` BIGINT NOT NULL,
  /*  */
  `for_entity_type` INTEGER NOT NULL,
  PRIMARY KEY (`for_entity_id`)
);
INSERT INTO entity_type (name) VALUES
("awa_vote")
;
