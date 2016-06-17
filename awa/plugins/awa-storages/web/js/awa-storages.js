/*
 *  awa-storages -- Storages and folders
 *  Copyright (C) 2012, 2016 Stephane Carrez
 *  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
var AWA = {};

function init_folder_list(id, idCreate, idCurrent, page_url) {
    $(id).list({
        actionId: null,
        itemPrefix: 'folder-',
        editUrl: contextPath + '/storages/forms/create-folder-form.html',
        currentItem: '#folder-' + idCurrent,
        selectAction: function(element, item) {
            var id = element.getSelectedId(item);
            return ASF.Update(this, contextPath + page_url + '?folderId=' + id, '#document-list-editor');
        }
    });
}
function init_upload(id, folder_id, file_id, upload_url) {
    var dr = new Dropzone(document.querySelector("#document-upload"), {
                thumbnailWidth: 80,
                thumbnailHeight: 80,
                clickable: "#upload-button",
                params: { uploadForm: "1", uploadButton: "1", folder: folder_id, id: file_id },
                paramName: "upload-file",
                url: upload_url,
                previewTemplate: '<div class="dz-preview dz-file-preview"><div class="dz-image"><img data-dz-thumbnail="" /></div><div class="dz-details\"><div class="dz-size"><span data-dz-size=""></span></div><div class="dz-filename"><span data-dz-name=""></span></div></div><div class="dz-progress"><span class="dz-upload" data-dz-uploadprogress=""></span></div><div class="dz-error-message"><span data-dz-errormessage=""></span></div><div class="dz-success-mark"></div><div class="dz-error-mark"></div></div>'
    });
    //  dr.on("sending", function(file) {
    //          document.querySelector("#total-progress").style.opacity = "1";
    //  });
    dr.on("success", function(file, response, e) {
        ASF.Execute($(id), response);
    });
}
