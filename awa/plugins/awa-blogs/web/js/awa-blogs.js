/*
 *  awa-blogs -- Blogs and post
 *  Copyright (C) 2016, 2017, 2019 Stephane Carrez
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
    $.widget("ui.blog_comment_list", $.ui.list, {
        currentNode: null,
        editInput: null,

        _create: function() {
            var self = this;

            $.ui.list.prototype._create.apply(this, arguments);
            if (self.options.selectAction === null) {
                self.options.selectAction = function(node, event) {
                    return self.selectAction(node, event);
                }
            }
        },
        getSelectedId: function(node) {
            while (node) {
                if ($(node).hasClass("awa-comment")) {
                    var id = $(node).attr('id');
                    return id == null ? null : id.substring(this.options.itemPrefix.length);
                }
                node = node.parentNode;
            }
            return null;
        },
        doPublish: function(node) {
            var id = this.getSelectedId(this.activeItem);

            return ASF.Post(this.activeItem, this.options.statusUrl, "id=" + id + "&status=COMMENT_PUBLISHED");
        },

        doSpam: function(node) {
            var id = this.getSelectedId(this.activeItem);

            return ASF.Post(this.activeItem, this.options.statusUrl, "id=" + id + "&status=COMMENT_SPAM");
        },

        doWait: function(node) {
            var id = this.getSelectedId(this.activeItem);

            return ASF.Post(this.activeItem, this.options.statusUrl, "id=" + id + "&status=COMMENT_WAITING");
        },

        editTask: function(node) {
            var list = this,
                id   = this.getSelectedId(this.activeItem),
                url  = this.options.editTaskUrl + "?id=" + id;

            ASF.Popup(node, 'vdo-edit-popup', url, {
                triggerHandler: function triggerHandler(action, node) {
                                    if (action == "open") {
                                        list.setMouseOver(false);
                                    } else {
                                        list.setMouseOver(true);
                                    }
                                },
                attachment: node
            });
            return false;
        },

        /**
         * Select the list item identified by <tt>node</tt> as the current selected item.
         *
         * @param node the list item to select
         */
        selectAction: function(node, event) {
            if ($(node).hasClass("comment-action-publish")) {
                return this.doPublish(node);
            } else if ($(node).hasClass("comment-action-spam")) {
                return this.doSpam(node);
            } else if ($(node).hasClass("comment-action-waiting")) {
                return this.doWait(node);
            } else if ($(node).hasClass("comment-action-delete")) {
                return this.enterDelete(node);
            } else {
                var t = event.target;

                if ($(t).hasClass("time-day")) {
                    return this.editTime($(t));
                } else {
                    return this._super("selectAction", node);
                }
            }
        }
    });

    /**
     * Editor configuration for the blog post.  Create and configure either the
     * MarkEdit text editor or the Wysiwyg editor with Trumbwowg.
     */
    $.widget("ui.blog_post_editor", {
        options: {
            syntax: "dotclear"
        },
        _create: function() {
            var self = this;
            var syntax = self.options.syntax;
            if (self.options.syntax == 'FORMAT_DOTCLEAR') {
                syntax = 'dotclear';
            }
            if (self.options.syntax == 'FORMAT_MEDIAWIKI') {
                syntax = 'mediawiki';
            }
            if (syntax == 'dotclear' || syntax == 'mediawiki') {

            // Create a MarkEdit editor on page load
                this.element.find('textarea').markedit({
                'preview': false,
                'syntax': syntax,
                'toolbar' : {
                    'backgroundMode': 'light',
                    'layout': 'bold italic underline strike | quote code | numberlist bulletlist heading line link image',
                    imageSelector: function(config, defaultValue, okCallback, cancelCallback) {
                        $('#image-selector').imageSelector({
                            autoOpen: false,
                            show: "blind",
                            hide: "explode",
                            minWidth: 900,
                            minHeight: 400,
                            modal: true,
                            buttons: [
                            { "text": "Select",
                               "click": function() {
                                    var value = $('#image-selector').imageSelector("value");
                                    okCallback(value);
                                    $(this).imageSelector("close");
                                }
                            }
                        ]}).imageSelector("openSelector");
                    }
                },
                });
            } else {
                this.element.find('textarea').trumbowyg({
                    minimalLinks: true,
                    autogrow: true,
                    imageWidthModalEdit: true,
                    autogrow: false,
                    resetCss: true,
                    tagsToRemove: ['script', 'link'],
                    btns: [['viewHTML'], ['undo', 'redo'],
                    ['formatting', '|', 'link', '|', 'image'],
                    ['strong', 'em', 'del'], ['insertImage'],
                    ['justifyLeft', 'justifyCenter', 'justifyRight', 'justifyFull'],
                    ['unorderedList', 'orderedList'],
                    ['preformatted'],
                    ['fontfamily'],
                    ['horizontalRule'],
                    ['removeformat'],
                    ['fullscreen']]
                });
            }
        }
    });

})(jQuery);

