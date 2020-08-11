/* Migrate from AWA 2.0 to AWA 2.1 */

ALTER TABLE awa_post CHANGE `summary` `summary` VARCHAR(4096) BINARY NOT NULL;
