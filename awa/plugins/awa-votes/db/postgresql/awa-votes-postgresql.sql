/* File generated automatically by dynamo */
/*  */
CREATE TABLE IF NOT EXISTS awa_rating (
  /* the rating identifier */
  "id" BIGINT NOT NULL,
  /* the rating taking into account all votes */
  "rating" INTEGER NOT NULL,
  /* the number of votes */
  "vote_count" INTEGER NOT NULL,
  /*  */
  "for_entity_id" BIGINT NOT NULL,
  /* the entity type */
  "for_entity_type" INTEGER NOT NULL,
  PRIMARY KEY ("id")
);
/* The vote table tracks a vote action by a user on a given database entity.
The primary key is made of the user, the entity id and entity type. */
CREATE TABLE IF NOT EXISTS awa_vote (
  /*  */
  "rating" INTEGER NOT NULL,
  /*  */
  "entity_id" BIGINT NOT NULL,
  /*  */
  "user_id" BIGINT NOT NULL,
  PRIMARY KEY ("entity_id", "user_id")
);
INSERT INTO entity_type (name) VALUES
('awa_rating'), ('awa_vote')
  ON CONFLICT DO NOTHING;
