/* File generated automatically by dynamo */
/* The jobs table. */
CREATE TABLE awa_jobs (
  /* the jobs id */
  `id` BIGINT PRIMARY KEY,
  /* the jobs version */
  `version` int ,
  /* the jobs name */
  `name` VARCHAR(256) ,
  /* the jobs creation date */
  `create_date` DATETIME ,
  /* the jobs start date */
  `start_date` DATETIME ,
  /* the jobs finish date */
  `finish_date` DATETIME ,
  /* the job status */
  `status` INTEGER NOT NULL,
  /* the job messages */
  `messages` TEXT NOT NULL,
  /* the job results */
  `results` TEXT NOT NULL,
  /* the user who triggered the job */
  `user_id` INTEGER ,
  /* the user session who triggered the job */
  `session_id` INTEGER ,
  /* the message creation event associated with this job */
  `event_id` INTEGER 
);
INSERT INTO entity_type (name) VALUES ("awa_jobs");
