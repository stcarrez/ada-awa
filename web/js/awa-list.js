/*
 *  awa-list -- List helpers
 *  Copyright (C) 2012 Stephane Carrez
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

(function($, undefined) {

    $.widget( "ui.list", $.ui.mouse, {

        min: 0,
        currentNode: null,

        /**
         * Relative URL to edit an item of the list.
         */
        editUrl: null,

        /**
         * Relative URL to refresh the item (after edit or view mode for example).
         */
        refreshUrl: null,

        /**
         * The active or highlighted item in the list.  This is the item which has the action element
         * as last child.
         */
        activeItem: null,

        /**
         * Disable the mouseover effect.  When set to 'true', the mouseover effect implemented by
         * setting the 'active' CSS class on the item is disabled.
         */
        disableMouseover: false,

        /**
         * Indicates whether the bloc is active (has the mouse focus).
         */
        isActive: false,

        /**
         * The current item
         */
        currentItem: null,

        /**
         * The current inline edit node.
         */
        currentEdit: null,

        /**
         * Prefix for item ids in the DOM tree
         */
        itemPrefix: '',

        _create: function() {
            var self = this;

            $.ui.mouse.prototype._create.apply(this, arguments);
            this.element.addClass("ui-list");
            this.element.bind('mouseover', function(event) {
                return self._mouseOver(event);
            }).bind('mouseout', function(event) {
                return self._mouseOut(event);
            }).bind('click', function(event) {
                return self.click(event);
            }).bind('blur', function(event) {
                return self.blur(event);
            });

            /**
             * Get the action element (a div in most cases) which contains the actions
             * to be displayed for the active/highlighted element.
             */
            this.action = $(this.options["actionId"]);
        },

        selectAction: function(node) {
            if (this.options.selectAction != null) {
                this.options.selectAction(this, node);
            }
            return false;
        },

        /**
         * Set the active item.  The active item is marked with the 'active' class and kept as reference
         * in the 'activeItem' member.
         *
         * @param item the active item or null to disable any active item
         */
        setActiveItem: function(item) {
            if (this.activeItem != null && this.activeItem[0] != item) {
                // Ensure that previously active line is deactivated.
                this.activeItem.removeClass("awa-active");
            }
            if (item != null) {
                this.activeItem = $(item);
                this.activeItem.addClass("awa-active");
            } else {
                this.activeItem = null;
            }
        },

        /**
         * Get the database or object ID of the selected node.
         *
         * @param node the node element that was selected
         * @return the ID of the object
         */
        getSelectedId: function(node) {
            if (node == null) {
                return null;
            }
            var id = node.id;
            if (id == null) {
                return null;
            }
            return id.substring(this.options.itemPrefix.length);
        },

        /**
         * Find the parent node that should receive the event.
         *
         */
        _getTargetNode: function(node) {
            while (node) {
                var name = node.tagName;
                if (name) {
                    name = name.toUpperCase();
                    if (name != "EM" && name != "I" && name != "B" && name != "IMG" && name != "SPAN") {
                        if (node.id && node.id != "" && this.action[0] != node) {
                            return node;
                        }
                        if (name == 'A') {
                            return node;
                        }
                    }
                }
                if (node == this.element[0]) {
                    return null;
                }

                node = node.parentNode;
            }
            return null;
        },

        getTarget: function(node) {
            if (node == this.element[0]) {
                return null;
            }
            if (node && (node.id == null || node.id == "")) {
                while (node) {
                    var name = node.tagName;
                    if (name) {
                        name = name.toUpperCase();
                        if (name == "DIV" || name == "DL") {
                            if (node.id && node.id != "")
                                break;
                        } else if (name == "A") {
                            break;
                        }
                    }
                    node = node.parentNode;
                    if (node == this.element[0]) {
                        return null;
                    }
                }
            }
            $("#current").html(node.id);
            return node;
        },

        _mouseOver: function(event) {
            var node = this._getTargetNode(event.target);
            if (node && this.currentNode != node) {
                $("#current").html("Mover " + node.id);
                this.setActiveItem(node);
                if (this.action[0]) {
                    this.action.detach();
                    this.action.prependTo(this.activeItem);
                }
            }
            if (this.isActive == false) {
                this.isActive = true;
                this.element.addClass("am-active");
            }
        } ,

        _mouseOut: function(event) {
            if (!this.disableMouseover) {
                var node = event.target;
                while (node && node != document) {
                    if (node == this.activeItem) {
                        this.setActiveItem(null);
                        $("#current").html("Mouse out");
                        break;
                    } else {
                        if (node == this.element[0]) {
                            break;
                        }
                        node = node.parentNode;
                    }
                }
                if (node && node != document) {
                    this.isActive = false;
                    this.element.removeClass("awa-active");
                }
            }
        },
        /**
         * Get the shopping category.
         */
        getCategoryId: function() {
            return $(this.element).attr("data-id");
        },
        enterEdit: function(event) {
            var node = this.element.find(".am-list");
            var catId = this.getCategoryId();
            $("#current").html("Enter edit");
            ASF.Update(this.element, "/am/shoplist/edit-category.html?id=" + catId, node);
        },
        enterDelete: function(event) {

        },
        enterCreate: function(event) {

        },
        click: function(event) {
            var node = this._getTargetNode(event.target);
            if (node && this.currentNode != node) {

                if ($(node).hasClass("am-edit")) {
                    this.enterEdit(event);
                } else if ($(node).hasClass("am-delete")) {
                    this.enterDelete(event);

                } else if ($(node).hasClass("awa-editable")) {
                    if (! $(node).hasClass("awa-editing")) {
                        this.currentEdit = node;
                        $(node).addClass("awa-editing");
                        this.enterCreate(event);
                    }
                } else {
                    this.selectAction(node);
                /* } else {
                    $("#current").html("Click " + node.id);
                    this.setActiveItem(node);
                    if (this.action[0]) {
                        this.action.detach();
                        this.action.prependTo(this.activeItem);
                    }
                    var title = $(node).find(".shop-title").html();
                    var count = $(node).find(".shop-count").html();
                    var edit = "<input type='text' value='" + count + ' ' + title + "' id='value'/>";
                    if (this.currentEdit) {
                        this.currentEdit.remove();
                    }
                    this.currentEdit = $("<div></div>");
                    this.currentEdit.addClass("am-shape am-inline-edit");
                    this.currentEdit.html(edit);
                    $(node).append(this.currentEdit);
                    $(node).find('input').focus();
                    var n = this;
                    $('html').bind('click', function(event) {
                        n.blur(event);
                        $("#current").html("Blur " + node.id);
                    }); */
                }
            } else {
                var name = event.target.tagName;
                node = event.target;

                if ($(node).hasClass("am-edit")) {
                    this.enterEdit(event);
                } else if ($(node).hasClass("am-delete")) {
                    this.enterDelete(event);
                }

            }
            event.stopPropagation();
        },
        blur: function(event) {
            if (this.currentEdit) {
                this.currentEdit.remove();
                this.currentEdit = null;
            }
        },
        destroy: function() {
            this.element.removeClass( "ui-list" );
            $.Widget.prototype.destroy.apply( this, arguments );
        }

    });

    $.extend($.ui.list, {
        version: "1.8.13"
    });

})( jQuery );
