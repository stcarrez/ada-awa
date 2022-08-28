ALTER TABLE awa_user ADD COLUMN `status` TINYINT NOT NULL DEFAULT 1;
INSERT OR IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = "awa_user"), "status");
