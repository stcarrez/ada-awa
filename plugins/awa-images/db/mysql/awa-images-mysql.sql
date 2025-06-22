/* File generated automatically by dynamo */
/* - The workspace contains one or several folders.
- Each image folder contains a set of images that have been uploaded by the user.
- An image can be visible if a user has an ACL permission to read the associated folder.
- An image marked as 'public=True' can be visible by anybody */
CREATE TABLE IF NOT EXISTS awa_image (
  /* the image identifier */
  `id` BIGINT NOT NULL,
  /* the image width */
  `width` INTEGER NOT NULL,
  /* the image height */
  `height` INTEGER NOT NULL,
  /* the thumbnail width */
  `thumb_width` INTEGER NOT NULL,
  /* the thumbnail height */
  `thumb_height` INTEGER NOT NULL,
  /*  */
  `path` VARCHAR(255) BINARY NOT NULL,
  /*  */
  `public` TINYINT NOT NULL,
  /*  */
  `version` INTEGER NOT NULL,
  /* the thumbnail storage */
  `thumbnail_id` BIGINT ,
  /* the folder where the image is stored */
  `folder_id` BIGINT NOT NULL,
  /* the user who uploaded the image */
  `owner_id` BIGINT NOT NULL,
  /* the image storage */
  `storage_id` BIGINT NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
INSERT IGNORE INTO ado_entity_type (name) VALUES
("awa_image");
INSERT IGNORE INTO ado_version (name, version) VALUES ("awa-images", 1);
