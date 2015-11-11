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
/*  */
CREATE TABLE awa_counter_definition (
  /* the counter name. */
  `name` VARCHAR(255) BINARY NOT NULL,
  /* the counter unique id. */
  `id` BIGINT NOT NULL,
  PRIMARY KEY (`id`)
);
INSERT INTO entity_type (name) VALUES
("awa_counter")
,("awa_counter_definition")
;
