/*
 *  awa-images -- Images
 *  Copyright (C) 2017 Stephane Carrez
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

    $.widget("ui.imageSelector", $.ui.dialog, {
        options: {
            /**
             * The URL to open the image selector.
             */
            url: "/storages/image-selector.html",

            /**
             * The URL to get the images of a folder.
             */
            imageListUrl: "/storages/lists/images.html",

            currentItem: null
        },

        _create: function() {
            var self = this;

            $.ui.dialog.prototype._create.apply(this, arguments);
        },

        /**
         * Open the image selector.
         *
         * @param node the inner element that was clicked
         * @return the voting node element (the default returns the ui.voting element)
         */
        openSelector: function() {
            var self = this;
            var url = contextPath + self.options.url;

            /* Perform the HTTP GET */
            jQuery.ajax({
                url: url,
                context: document.body,
                success: function(data, status, jqXHDR) {
                    var contentType = jqXHDR.getResponseHeader('Content-type');
                    if (contentType == null) {
                        contentType = "text/html";
                    }
                    if (contentType.match(/^text\/(html|xml)(;.*)?$/i)) {
                        $(self.element).html(jqXHDR.responseText);

                        /**
                         * Extract a title from the inner form to setup the dialog box.
                         */
                        var dTitle, dBox = $(self.element).children('div');
                        if (dBox.length == 0) {
                            dTitle = $(self.element).children('h2');
                        } else {
                            dTitle = $(dBox).children('h2');
                        }
                        if (dTitle.length > 0) {
                            self._setOption("title", dTitle.html());
                            dTitle.remove();
                        } else {
                            dTitle = $(self.element).children('div').attr('title');
                            if (dTitle != null) {
                                self._setOption("title", dTitle );
                            }
                        }
                        self.folderList = $(self.element).find(".awa-folder-list-editor");
                        self.imageList = $(self.element).find(".awa-image-list-editor");
                        self.update();
                        self.open();
                        $("body").css({ overflow: 'hidden' });
                    }
                },
                error: function(jqXHDR, status, error) {
                    ASF.AjaxError(jqXHDR, status, error, self.element);
                }
            });
        },
        close: function() {
            var self = this;
            $("body").css({ overflow: 'inherit' })
            self._super();
        },
        update: function() {
            var self = this;
            $(self.folderList).list({
                actionId: null,
                itemPrefix: 'folder-',
                editUrl: contextPath + '/storages/forms/create-folder-form.html',
                currentItem: '#folder-',
                selectAction: function(element, item) {
                    var id = element.getSelectedId(item);
                    self.options.currentItem = null;
                    return ASF.Update(this, contextPath + self.options.imageListUrl + '?folderId=' + id,
                                      $(self.imageList));
                }
            });
            $(self.imageList).bind('click', function(event) {
                return self.click(event);
            });
        },
        click: function(event) {
            var self = this;
            var node = event.target;
            while (node) {
                var name = node.tagName;
                if (name) {
                    name = name.toUpperCase();
                    if (name == "DIV") {
                        if (node.id && node.id != "") {
                            if (self.options.currentItem != null) {
                                $(self.options.currentItem).removeClass("active");
                            }
                            $(node).addClass("active");
                            self.options.currentItem = node;
                            return false;
                        }
                    }
                }
                if (node == this.element[0]) {
                    return false;
                }
                node = node.parentNode;
            }
            return false;
        },
        value: function() {
            if (this.options.currentItem != null) {
                var name = $(this.options.currentItem).find("span").text();
                var folder = $(this.folderList).find("div.asf-selected div").text();
                return folder + "/" + name;
            } else {
                return "";
            }
        }
    });
})( jQuery );
