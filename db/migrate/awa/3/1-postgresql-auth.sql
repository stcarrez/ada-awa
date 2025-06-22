CREATE TABLE IF NOT EXISTS awa_authenticate (
  /* the identifier */
  "id" BIGINT NOT NULL,
  /* the optimistic lock version. */
  "version" INTEGER NOT NULL,
  /* the identification string */
  "ident" VARCHAR(255) NOT NULL,
  /* the optional salt */
  "salt" VARCHAR(255) NOT NULL,
  /* the optional hash */
  "hash" VARCHAR(255) NOT NULL,
  /* the authenticate method */
  "method" SMALLINT NOT NULL,
  /* the email that we authenticate */
  "email_id" BIGINT NOT NULL,
  /* the user that is authenticated */
  "user_id" BIGINT NOT NULL,
  PRIMARY KEY ("id")
);
INSERT INTO ado_entity_type (name) VALUES ("awa_authenticate") ON CONFLICT DO NOTHING;

INSERT INTO awa_authenticate (`id`, `version`, `ident`, `salt`, `hash`, `method`, `email_id`, `user_id`)
  SELECT user.id, 1, user.open_id, '', '', 1, user.email_id, user.id FROM awa_user AS user
     WHERE user.open_id != '';

INSERT INTO awa_authenticate (`id`, `version`, `ident`, `salt`, `hash`, `method`, `email_id`, `user_id`)
  SELECT user.id, 1, email.email, user.salt, user.password, 0, user.email_id, user.id FROM awa_user AS user
  INNER JOIN awa_email AS email ON user.email_id = email.id
  WHERE user.open_id = '';

INSERT INTO `ado_sequence` (name, version, value, block_size)
  SELECT 'awa_authenticate', 1, COALESCE(MAX(auth.id), 0) + 1, 100 FROM awa_authenticate AS auth;

ALTER TABLE awa_session ADD COLUMN `user_auth_id` BIGINT;

ALTER TABLE awa_user DROP COLUMN `password`;
ALTER TABLE awa_user DROP COLUMN `open_id`;
ALTER TABLE awa_user DROP COLUMN `salt`;

