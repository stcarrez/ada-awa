/* File generated automatically by dynamo */
/*  */
CREATE TABLE awa_counter (
  /* the object associated with the counter. */
  `object_id` BIGINT NOT NULL,
  /* the day associated with the counter. */
  `date` DATE NOT NULL,
  /* the counter value. */
  `counter` INTEGER NOT NULL,
  /* the definition id. */
  `definition_id` INTEGER NOT NULL,
  PRIMARY KEY (`object_id`, `date`, `definition_id`)
);
/* A counter definition defines what the counter represents. It uniquely identifies
the counter for the Counter table. A counter may be associated with a database
table. In that case, the counter definition has a relation to the corresponding Entity_Type. */
CREATE TABLE awa_counter_definition (
  /* the counter name. */
  `name` VARCHAR(255) NOT NULL,
  /* the counter unique id. */
  `id` BIGINT NOT NULL,
  /*  */
  `entity_type` INTEGER ,
  PRIMARY KEY (`id`)
);
INSERT INTO entity_type (name) VALUES ("awa_counter");
INSERT INTO entity_type (name) VALUES ("awa_counter_definition");
