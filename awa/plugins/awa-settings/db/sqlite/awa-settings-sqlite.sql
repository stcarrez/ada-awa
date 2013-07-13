/* File generated automatically by dynamo */
/* The global setting holds some generic
application configuration parameter
which can be stored in the database.

The global setting can be specific to a server. */
CREATE TABLE awa_global_setting (
  /* the global setting identifier. */
  `id` BIGINT PRIMARY KEY,
  /* the global setting value. */
  `value` VARCHAR(255) NOT NULL,
  /* the global setting optimistic lock version. */
  `version` INTEGER NOT NULL,
  /* the server to which this global setting applies. */
  `server_id` INTEGER NOT NULL,
  /* the setting that corresponds to this global setting. */
  `setting_id` BIGINT NOT NULL
);
/* The setting table defines all the possible settings
that an application manages.  This table is automatically
populated when an application starts. It is not modified.
 */
CREATE TABLE awa_setting (
  /* the setting identifier. */
  `id` BIGINT PRIMARY KEY,
  /* the setting name. */
  `name` VARCHAR(255) NOT NULL
);
/* The user setting holds the setting value for a given user.
It is created the first time a user changes the default
setting value. It is updated when the user modifies the setting.

 */
CREATE TABLE awa_user_setting (
  /* the user setting identifier. */
  `id` BIGINT PRIMARY KEY,
  /* the setting value. */
  `value` VARCHAR(255) NOT NULL,
  /* the setting optimistic lock version. */
  `version` INTEGER NOT NULL,
  /* the setting that correspond to the value. */
  `setting_id` BIGINT NOT NULL,
  /* the user to which the setting value is associated. */
  `user_id` BIGINT NOT NULL
);
INSERT INTO entity_type (name) VALUES ("awa_global_setting");
INSERT INTO entity_type (name) VALUES ("awa_setting");
INSERT INTO entity_type (name) VALUES ("awa_user_setting");
