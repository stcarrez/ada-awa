/* Database schema version (per module) */
CREATE TABLE IF NOT EXISTS ado_version (
  /* the module name */
  `name` VARCHAR(127) UNIQUE NOT NULL,
  /* the database version schema for this module */
  `version` INTEGER NOT NULL,
  PRIMARY KEY (`name`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
INSERT IGNORE INTO ado_entity_type (name) VALUES ("ado_version");

