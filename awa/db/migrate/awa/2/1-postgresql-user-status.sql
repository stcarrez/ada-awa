ALTER TABLE awa_user ADD COLUMN "status" SMALLINT NOT NULL DEFAULT 1;
INSERT INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = 'awa_user'), 'status')
  ON CONFLICT DO NOTHING;
