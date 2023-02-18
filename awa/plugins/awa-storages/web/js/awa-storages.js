/*
 *  awa-storages -- Storages and folders
 *  Copyright (C) 2012, 2016, 2018, 2019 Stephane Carrez
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
(function($, undefined) {

    /**
     * @class ui.uploader
     *
     * Upload button
     */
    $.widget("ui.uploader", {
        options: {
            /**
             * @cfg {String} acceptedFiles the list of mime types which are accepted.
             */
            acceptedFiles: null,

            /**
             * @cfg {String} folderId the current folder id.
             */
            folderId: null,

            /**
             * @cfg {String} fileId the file id.
             */
            fileId: null
        },

        _create: function() {
            var self = this;

            this.element.on('click', function(event) {
                self.uploadDialog(self.options.folderId, self.options.fileId);
                return false;
            });
        },
        uploadDialog: function(folderId, fileId) {
            var self = this;
            var div = document.createElement("div");
            div.id = 'upload-dialog';
            div = $(div);

            var d = $(document.body);
            if (d.length == 0) {
                return false;
            }

            $(d).append($(div));

            var params = "?folderId=" + folderId;
            if (fileId != "") {
               params += "&id=" + fileId;
            }

            /* Perform the HTTP GET */
            jQuery.ajax({
                url: this.options.uploadUrl + params,
                context: document.body,
                success: function(data, status, jqXHDR) {
                    var contentType = jqXHDR.getResponseHeader('Content-type');
                    if (contentType == null) {
                        contentType = "text/html";
                    }
                    if (contentType.match(/^text\/(html|xml)(;.*)?$/i)) {
                        $(div).dialog({
                            autoOpen: false,
                            show: "blind",
                            hide: "explode",
                            minWidth: 600,
                            close: function() {
                                $(div).dialog('destroy');
                                $(div).remove();
                            }
                        });

                        $(div).html(jqXHDR.responseText);

                        /**
                        * Extract a title from the inner form to setup the dialog box.
                        */
                        var dTitle, dBox = $(div).children('div');
                        if (dBox.length == 0) {
                            dTitle = $(div).children('h2');
                        } else {
                            dTitle = $(dBox).children('h2');
                        }
                        if (dTitle.length > 0) {
                            $(div).dialog("option", "title", dTitle.html());
                            dTitle.remove();
                        } else {
                            dTitle = $(div).children('div').attr('title');
                            if (dTitle != null) {
                                $(div).dialog("option", "title", dTitle );
                                /* $(div).attr('title', title); */
                            }
                        }
                        $(div).dialog('open');
                        $("#uploadForm").dropzone({
                            url: self.options.uploadUrl,
                            dictDefaultMessage: self.options.dictDefaultMessage,
                            dictInvalidFileType: self.options.dictInvalidFileType,
                            acceptedFiles: self.options.acceptedFiles,
                            maxFiles: 1,
                            paramName: "upload-file",
                            params: function(files, xhr, chunk) {
                                return { "upload-button": "1", "uploadForm": "1" }
                            },
                            success: function(file, response) {
                                ASF.Execute(self.currentNode, response);
                                $(div).dialog('close');
                                if (self.options.uploadDoneAction) {
                                    self.options.uploadDoneAction();
                                }
                            }
                        });

                    } else if (contentType.match(/^application\/json(;.*)?$/i)) {
                        ASF.Execute(self.currentNode, data);
                    }
                },
                error: function(jqXHDR, status, error) {
                    ASF.AjaxError(jqXHDR, status, error, self.element);
                }
            });
        }
    });

    /**
     * @class ui.viewer
     *
     * Upload button
     */
    $.widget("ui.viewer", {
        options: {
            url: null,

            /**
             * @cfg {String} fileId the file id.
             */
            fileId: null
        },

        _create: function() {
            var self = this;

            self.displayFile(self.options.url);
        },
        displayFile: function(url) {
            var self = this;

            /* Perform the HTTP GET */
            jQuery.ajax({
                url: url,
                dataType: 'text',
                context: document.body,
                success: function(data, status, jqXHDR) {
                    var contentType = jqXHDR.getResponseHeader('Content-type');
                    if (contentType == null) {
                        contentType = "text/html";
                    }
                    if (contentType.match(/^text\/(html|xml)(;.*)?$/i)) {
                        $(self.element).text(data);

                    } else {
                        $(self.element).text(data);
                    }
                },
                error: function(jqXHDR, status, error) {
                    ASF.AjaxError(jqXHDR, status, error, d);
                }
            });
        }
    });

    $.widget("ui.folder", $.ui.list, {
        options: {
            actionId: null,
            itemPrefix: 'folder-',
            acceptedFiles: null
        },
        selectedItem: null,

        _create: function() {
            var self = this;
            var upload = $(self.options.uploadButton);

            $.ui.list.prototype._create.apply(this, arguments);
            $(upload).on('click', function(event) {
                var id = self.getSelectedId(self.selectedItem);
                if (id != null) {
                    self.uploadDialog(id);
                }
                return false;
            });
            self.options.selectAction = self.selectAction;
        },
        selectAction: function(node, event) {
            var id = this.getSelectedId(node);
            if (this.selectedItem) {
                $(this.selectedItem).removeClass(this.options.selectClass);
            }
            this.currentNode = node;
            this.selectedItem = node;
            $(this.selectedItem).addClass(this.options.selectClass);
            if (this.options.folderAction) {
                return this.options.folderAction(id);
            }
            return ASF.Update(this, this.options.selectUrl + '?folderId=' + id, '#storage-list-editor');
        },

        uploadDialog: function(folderId) {
            var self = this;
            var div = document.createElement("div");
            div.id = 'upload-dialog';
            div = $(div);

            var d = $(document.body);
            if (d.length == 0) {
                return false;
            }

            $(d).append($(div));

            /* Perform the HTTP GET */
            jQuery.ajax({
                url: this.options.uploadUrl + "?folderId=" + folderId,
                context: document.body,
                success: function(data, status, jqXHDR) {
                    var contentType = jqXHDR.getResponseHeader('Content-type');
                    if (contentType == null) {
                        contentType = "text/html";
                    }
                    if (contentType.match(/^text\/(html|xml)(;.*)?$/i)) {
                        $(div).dialog({
                            autoOpen: false,
                            show: "blind",
                            hide: "explode",
                            minWidth: 600,
                            close: function() {
                                $(div).dialog('destroy');
                                $(div).remove();
                            }
                        });

                        $(div).html(jqXHDR.responseText);

                        /**
                        * Extract a title from the inner form to setup the dialog box.
                        */
                        var dTitle, dBox = $(div).children('div');
                        if (dBox.length == 0) {
                            dTitle = $(div).children('h2');
                        } else {
                            dTitle = $(dBox).children('h2');
                        }
                        if (dTitle.length > 0) {
                            $(div).dialog("option", "title", dTitle.html());
                            dTitle.remove();
                        } else {
                            dTitle = $(div).children('div').attr('title');
                            if (dTitle != null) {
                                $(div).dialog("option", "title", dTitle );
                                /* $(div).attr('title', title); */
                            }
                        }
                        $(div).dialog('open');
                        $("#uploadForm").dropzone({
                            url: self.options.uploadUrl,
                            dictDefaultMessage: self.options.dictDefaultMessage,
                            dictInvalidFileType: self.options.dictInvalidFileType,
                            acceptedFiles: self.options.acceptedFiles,
                            maxFiles: 1,
                            paramName: "upload-file",
                            params: function(files, xhr, chunk) {
                                return { "upload-button": "1", "uploadForm": "1" }
                            },
                            success: function(file, response) {
                                ASF.Execute(self.currentNode, response);
                                $(div).dialog('close');
                            }
                        });

                    } else if (contentType.match(/^application\/json(;.*)?$/i)) {
                        ASF.Execute(self.currentNode, data);
                    }
                },
                error: function(jqXHDR, status, error) {
                    ASF.AjaxError(jqXHDR, status, error, self.element);
                }
            });
        }
    });

})( jQuery );
