/* File generated automatically by dynamo */
/* The job is associated with a dispatching queue. */
CREATE TABLE IF NOT EXISTS awa_job (
  /* the job identifier */
  "id" BIGINT NOT NULL,
  /* the job status */
  "status" SMALLINT NOT NULL,
  /* the job name */
  "name" VARCHAR(255) NOT NULL,
  /* the job start date */
  "start_date" TIMESTAMP ,
  /* the job creation date */
  "create_date" TIMESTAMP NOT NULL,
  /* the job finish date */
  "finish_date" TIMESTAMP ,
  /* the job progress indicator */
  "progress" INTEGER NOT NULL,
  /* the job parameters */
  "parameters" TEXT NOT NULL,
  /* the job result */
  "results" TEXT NOT NULL,
  /*  */
  "version" INTEGER NOT NULL,
  /* the job priority */
  "priority" INTEGER NOT NULL,
  /*  */
  "user_id" BIGINT ,
  /*  */
  "event_id" BIGINT ,
  /*  */
  "session_id" BIGINT ,
  PRIMARY KEY ("id")
);
INSERT INTO ado_entity_type (name) VALUES
('awa_job')
  ON CONFLICT DO NOTHING;
