/* File generated automatically by dynamo */
/* - The workspace contains one or several folders.
- Each image folder contains a set of images that have been uploaded by the user.
- An image can be visible if a user has an ACL permission to read the associated folder.
- An image marked as 'public=True' can be visible by anybody */
CREATE TABLE IF NOT EXISTS awa_image (
  /* the image identifier */
  "id" BIGINT NOT NULL,
  /* the image width */
  "width" INTEGER NOT NULL,
  /* the image height */
  "height" INTEGER NOT NULL,
  /* the thumbnail width */
  "thumb_width" INTEGER NOT NULL,
  /* the thumbnail height */
  "thumb_height" INTEGER NOT NULL,
  /*  */
  "path" VARCHAR(255) NOT NULL,
  /*  */
  "public" BOOLEAN NOT NULL,
  /*  */
  "version" INTEGER NOT NULL,
  /*  */
  "thumbnail_id" BIGINT ,
  /*  */
  "folder_id" BIGINT NOT NULL,
  /*  */
  "owner_id" BIGINT NOT NULL,
  /*  */
  "storage_id" BIGINT NOT NULL,
  PRIMARY KEY ("id")
);
INSERT INTO ado_entity_type (name) VALUES
('awa_image')
  ON CONFLICT DO NOTHING;
INSERT INTO ado_version (name, version)
  VALUES ("awa-images", 1)
  ON CONFLICT DO NOTHING;
