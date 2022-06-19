/*
 *  awa-wikis -- Wiki editor support
 *  Copyright (C) 2016 - 2022 Stephane Carrez
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
     * Editor configuration for the blog post.  Create and configure either the
     * MarkEdit text editor or the Wysiwyg editor with Trumbwowg.
     */
    $.widget("ui.wiki_editor", {
        options: {
            syntax: "markdown"
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
            if (self.options.syntax == 'FORMAT_MARKDOWN') {
                syntax = 'markdown';
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
                }).each(function () {
                    this.setAttribute("style", "height:" + (this.scrollHeight) + "px;overflow-y:hidden;");
                }).on("input", function () {
                    this.style.height = "auto";
                    this.style.height = (this.scrollHeight) + "px";
                });
            } else if (syntax == 'markdown') {

                // Create a MarkEdit editor on page load
                this.element.find('textarea').each(function() {
                    const easyMDE = new EasyMDE({
                        element: this,
                        autoDownloadFontAwesome: false,
                        spellChecker: false,
                        indentWithTags: false,
                        autofocus: true,
                        tabSize: 4,
                        lineNumbers: true,
                        renderingConfig: {
                            singleLineBreaks: false
                        },
                        toolbar: [
                            'bold', 'italic', 'strikethrough', '|',
                            'heading-1', 'heading-2', 'heading-3', '|',
                            'code', 'quote', '|',
                            'unordered-list', 'ordered-list', '|',
                            'link', {
                                name: 'image',
                                action(e) {
                                    const cm = e.codemirror;
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
                                                  cm.replaceSelection('![' + value + '](' + value + ')');
                                                  cm.focus();
                                                  $(this).imageSelector("close");
                                              }
                                            }
                                        ]}).imageSelector("openSelector");
                                },
                                className: 'fa fa-image',
                                title: 'Add image'
                            },
                            'table', 'horizontal-rule', '|', 'clean-block'
                        ]
                    });
                });
            } else {
                this.element.find('textarea').trumbowyg({
                    minimalLinks: true,
                    imageWidthModalEdit: true,
                    autogrow: true,
                    resetCss: false,
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

