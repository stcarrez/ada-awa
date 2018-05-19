# Images Module
The image plugin is an extension to the storage plugin that identifies images and
provides thumbnails as well as resizing of the original image.

## Image Module
The <tt>Image_Module</tt> type represents the image module.  An instance of the image
module must be declared and registered when the application is created and initialized.
The image module is associated with the image service which provides and implements
the image management operations.

When the image module is initialized, it registers itself as a listener to the storage
module to be notified when a storage file is created, updated or removed.  When a file
is added, it looks at the file type and extracts the image information if the storage file
is an image.

| Name                      | Description                                                    |
|:--------------------------|:---------------------------------------------------------------|
|images.thumbnail_command|The command to execute to generate an image thumbnail for the Images module.|
| |convert -verbose -resize \#{width}x\#{height} -background white -gravity center -extent \#{width}x\#{height} -format jpg -quality 75 \#{src} \#{dst}|



## Ada Beans
The `Image_List_Bean` type is used to represent a list of image stored in
a folder.

The `Image_Bean` type holds all the data to give information about an image.




#### AWA.Images.Models.Image_Bean

The information about an image.

| Type     | Ada      | Name       | Description                                             |
|:---------|:---------|:-----------|:--------------------------------------------------------|
||Identifier|folder_id|the image folder identifier.|
||String|folder_name|the image folder name.|
||Identifier|id|the image file identifier.|
||String|name|the image file name.|
||Date|create_date|the file creation date.|
||String|uri|the file storage URI.|
||AWA.Storages.Models.Storage_Type|storage|the file storage URI.|
||String|mime_type|the file mime type.|
||Integer|file_size|the file size.|
||Boolean|is_public|whether the image is public.|
||Integer|width|the image width.|
||Integer|height|the image height.|





#### AWA.Images.Models.Image_Info

The list of images for a given folder.

| Type     | Ada      | Name       | Description                                             |
|:---------|:---------|:-----------|:--------------------------------------------------------|
||Identifier|id|the storage identifier which contains the image data.|
||String|name|the image file name.|
||Date|create_date|the image file creation date.|
||String|uri|the image file storage URI.|
||Integer|storage|the image file storage URI.|
||String|mime_type|the image file mime type.|
||Integer|file_size|the image file size.|
||Integer|width|the image width.|
||Integer|height|the image height.|
||Integer|thumb_width|the image thumbnail width.|
||Integer|thumb_height|the image thumbnail height.|
||Identifier|thumbnail_id|the image thumbnail identifier.|





## Queries
| Name              | Description                                                           |
|:------------------|:----------------------------------------------------------------------|
|image-info|Get the description of an image.|


| Name              | Description                                                           |
|:------------------|:----------------------------------------------------------------------|
|image-list|Get a list of images for a given folder.|



## Data model
![](images/awa_images_model.png)


