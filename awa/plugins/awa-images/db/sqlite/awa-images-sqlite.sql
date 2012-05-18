/* File generated automatically by dynamo */
/* An image that was uploaded by a user in an image folder. */
CREATE TABLE awa_image (
  /* the image identifier. */
  `id` BIGINT PRIMARY KEY,
  /* the image version. */
  `version` int ,
  /* the image width. */
  `width` INTEGER NOT NULL,
  /* the image height. */
  `height` INTEGER NOT NULL,
  /* the task within the server which is processing this message */
  `task_id` INTEGER NOT NULL,
  /* the image name. */
  `name` VARCHAR(255) NOT NULL,
  /* the image type. */
  `mime_type` VARCHAR(255) NOT NULL,
  /* the image path. */
  `path` VARCHAR(255) NOT NULL,
  /* the image creation date. */
  `create_date` DATETIME NOT NULL,
  /* the original image if this image was created by the application. */
  `original_id` INTEGER NOT NULL,
  /* the thumbnail image to display the image is an image selector. */
  `thumbnail_id` INTEGER NOT NULL,
  /* the user who uploaded the image. */
  `user_id` INTEGER NOT NULL,
  /* the image folder where this image is stored. */
  `folder_id` INTEGER NOT NULL,
  /* the image data if the storage type is DATABASE. */
  `image_id` INTEGER 
);
/* The image folder contains a set of images that have been uploaded by the user. */
CREATE TABLE awa_image_folder (
  /* the image folder identifier */
  `id` INTEGER PRIMARY KEY AUTOINCREMENT,
  /* the image folder version. */
  `version` int ,
  /* the image folder name */
  `name` VARCHAR(256) NOT NULL,
  /* the image folder creation date */
  `create_date` DATETIME NOT NULL,
  /* the user who owns this image folder */
  `user_id` INTEGER NOT NULL,
  /* the workspace that this image folder belongs. */
  `workspace_id` INTEGER NOT NULL
);
INSERT INTO entity_type (name) VALUES ("awa_image");
INSERT INTO entity_type (name) VALUES ("awa_image_folder");
