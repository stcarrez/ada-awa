/* Copied from awa-jobs-mysql.sql*/
/* File generated automatically by dynamo */
/* The job is associated with a dispatching queue. */
CREATE TABLE awa_job (
  /* the job identifier */
  `id` BIGINT NOT NULL,
  /* the job status */
  `status` TINYINT NOT NULL,
  /* the job name */
  `name` VARCHAR(255) BINARY NOT NULL,
  /* the job start date */
  `start_date` DATETIME ,
  /* the job creation date */
  `create_date` DATETIME NOT NULL,
  /* the job finish date */
  `finish_date` DATETIME ,
  /* the job progress indicator */
  `progress` INTEGER NOT NULL,
  /* the job parameters */
  `parameters` TEXT NOT NULL,
  /* the job result */
  `results` TEXT NOT NULL,
  /*  */
  `version` INTEGER NOT NULL,
  /* the job priority */
  `priority` INTEGER NOT NULL,
  /*  */
  `user_id` BIGINT ,
  /*  */
  `event_id` BIGINT ,
  /*  */
  `session_id` BIGINT ,
  PRIMARY KEY (`id`)
);
INSERT INTO entity_type (name) VALUES
("awa_job")
;

INSERT INTO awa_wiki_space 
  (id, name, is_public, create_date, version, workspace_id, left_side, right_side, format)
VALUES
  (1, 'Atlas', 1, '2015-12-05 16:13:02', 1, 1, '[Main]', '', 2);

set @acl_id = (SELECT MAX(awa_acl.id)+1 FROM awa_acl);
INSERT INTO awa_acl (id, entity_type, user_id, entity_id, writeable)
  SELECT @acl_id:=@acl_id+1, entity_type.id, awa_user.id, 1, 0
  FROM awa_user, entity_type
  WHERE entity_type.name = 'awa_wiki_space';
