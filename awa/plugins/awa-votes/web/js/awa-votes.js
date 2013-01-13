/*
 *  awa-votes -- Votes
 *  Copyright (C) 2013 Stephane Carrez
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

    $.widget("ui.votes", $.ui.mouse, {
        options: {
            /**
             * The CSS class used to vote up.
             */
            voteUpClass: "vote-up",

            /**
             * The CSS class indicating the user voted +1 already.
             */
            userVoteUpClass: "user-vote1",

            /**
             * The CSS class used to vote down.
             */
            voteDownClass: "vote-down",

            /**
             * The CSS class indicating the user voted -1 already.
             */
            userVoteDownClass: "user-vote-1",

            /**
             * URL to vote for the item.
             */
            voteUrl: null,

            itemPrefix: "vote-"
        },

        _create: function() {
            var self = this;

            $.ui.mouse.prototype._create.apply(this, arguments);
            this.element.bind('click', function(event) {
                return self.click(event);
            });
        },

        /**
         * Get the voting node element.
         *
         * @param node the inner element that was clicked
         * @return the voting node element (the default returns the ui.voting element)
         */
        getVotingNode: function(node) {
            return this.element;
        },

        /**
         * Get the object ID of the selected node.  This operation assumes that the HTML element has
         * an id of the form 'itemPrefix' + object Id.  This operation removes the item prefix.
         *
         * @param node the node element that was selected
         * @return the ID of the object
         */
        getSelectedId: function(node) {
            if (node == null) {
                return null;
            }
            var id = $(node).attr('id');
            return id == null ? null : id.substring(this.options.itemPrefix.length);
        },

        /**
         * Find the parent node that should receive the event.
         *
         */
        getTargetNode: function(node) {
            while (node) {
                var name = node.tagName;
                if (name) {
                    name = name.toUpperCase();
                    if (name != "EM" && name != "I" && name != "B" && name != "IMG" && name != "SPAN") {
                        return node;
                    }
                }
                if (node == this.element[0]) {
                    return null;
                }

                node = node.parentNode;
            }
            return null;
        },

        /**
         * Vote for the item.
         *
         * @param event the event that triggered the vote
         * @param node the vote node
         * @param rating the user rating (vote-up: +1, undo: 0, vote-down: -1)
         */
        vote: function(event, node, rating) {
            var id = this.getSelectedId(node),
                url = this.options.voteUrl + id + "&rating=" + rating;

            /* Perform the HTTP POST */
            jQuery.ajax({
                type: "POST",
                url: url,
                context: document.body,
                success: function(data, status, jqXHDR) {
                    var contentType = jqXHDR.getResponseHeader('Content-type');
                    if (contentType == null) {
                        contentType = "text/html";
                    }
                    if (contentType.match(/^text\/(html|xml)(;.*)?$/i)) {
                        $(node).html(jqXHDR.responseText);

                    } else if (contentType.match(/^application\/json(;.*)?$/i)) {
                        ASF.Execute(node, data);
                    }
                }
            });
        },

        /**
         * Click action executed when one inner HTML element is clicked.
         *
         * @param event the event that was clicked
         */
        click: function(event) {
            var voting, node = this.getTargetNode(event.target);
            if (node) {
                voting = this.getVotingNode(node);
                if ($(node).hasClass(this.options.voteUpClass)) {
                    this.vote(event, voting, $(voting).hasClass(this.options.userVoteUpClass) ? 0 : 1);
                } else if ($(node).hasClass(this.options.voteDownClass)) {
                    this.vote(event, voting, $(voting).hasClass(this.options.userVoteDownClass) ? 0 : -1);
                }
            }
            event.stopPropagation();
        }
    });
})( jQuery );
