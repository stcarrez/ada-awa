/*
 * Migration for Atlas 0.2 to 0.3
 *
 * AWA migration from 0.2 to 0.3 must be executed separately.
 */
ALTER TABLE mblog CHANGE COLUMN `create_date` `creation_date` DATETIME NOT NULL;

