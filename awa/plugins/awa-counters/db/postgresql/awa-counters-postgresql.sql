/* File generated automatically by dynamo */
/*  */
CREATE TABLE IF NOT EXISTS awa_counter (
  /* the object associated with the counter. */
  "object_id" BIGINT NOT NULL,
  /* the day associated with the counter. */
  "date" DATE NOT NULL,
  /* the counter value. */
  "counter" INTEGER NOT NULL,
  /* the counter definition identifier. */
  "definition_id" BIGINT NOT NULL,
  PRIMARY KEY ("object_id", "date", "definition_id")
);
/* A counter definition defines what the counter represents. It uniquely identifies
the counter for the Counter table. A counter may be associated with a database
table. In that case, the counter definition has a relation to the corresponding Entity_Type. */
CREATE TABLE IF NOT EXISTS awa_counter_definition (
  /* the counter name. */
  "name" VARCHAR(255) NOT NULL,
  /* the counter unique id. */
  "id" INTEGER NOT NULL,
  /* the optional entity type that identifies the database table. */
  "entity_type" INTEGER ,
  PRIMARY KEY ("id")
);
/*  */
CREATE TABLE IF NOT EXISTS awa_visit (
  /* the entity identifier. */
  "object_id" BIGINT NOT NULL,
  /* the number of times the entity was visited by the user. */
  "counter" INTEGER NOT NULL,
  /* the date and time when the entity was last visited. */
  "date" TIMESTAMP NOT NULL,
  /* the user who visited the entity. */
  "user" BIGINT NOT NULL,
  /* the counter definition identifier. */
  "definition_id" BIGINT NOT NULL,
  PRIMARY KEY ("object_id", "user", "definition_id")
);
INSERT INTO ado_entity_type (name) VALUES
('awa_counter'), ('awa_counter_definition'), ('awa_visit')
  ON CONFLICT DO NOTHING;
