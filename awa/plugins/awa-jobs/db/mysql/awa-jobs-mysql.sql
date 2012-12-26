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
  `parameters` VARCHAR(255) BINARY NOT NULL,
  /* the job result */
  `results` VARCHAR(255) BINARY NOT NULL,
  /* the server identifier where the job is running */
  `server_id` INTEGER ,
  /* the task identifier on the server which executes the job */
  `task_id` INTEGER ,
  /*  */
  `version` INTEGER NOT NULL,
  /* the job priority */
  `priority` INTEGER NOT NULL,
  /*  */
  `event_id` BIGINT NOT NULL,
  /*  */
  `user_id` BIGINT ,
  /*  */
  `job_queue_id` BIGINT NOT NULL,
  /*  */
  `session_id` BIGINT ,
  PRIMARY KEY (`id`)
);
INSERT INTO entity_type (name) VALUES
("awa_job")
;
