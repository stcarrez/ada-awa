ALTER TABLE sequence RENAME TO ado_sequence;
ALTER TABLE entity_type RENAME TO ado_entity_type;
UPDATE ado_entity_type SET name = 'ado_entity_type' WHERE name = 'entity_type';
UPDATE ado_entity_type SET name = 'ado_sequence' WHERE name = 'sequence';
