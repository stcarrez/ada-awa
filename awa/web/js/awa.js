/*
 *  awa -- AWA helpers
 *  Copyright (C) 2014, 2018, 2020 Stephane Carrez
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
var AWA = AWA || {};

/**
 * @brief A modal dialog box.
 */
jQuery.fn.extend({
    modal: function(options) {
		// if nothing is selected, return nothing; can't chain anyway
		if (!this.length) {
			return null;
		}

		var modal = $.data(this[0], "modal");
		if (!modal) {
		    modal = new $.modal(options, this[0]);
		    $.data(this[0], "modal", modal);
		}
		return modal;
	}
});

$.modal = function(options, field) {
	this.settings = $.extend(true, {}, $.modal.defaults, options);
	this.currentField = field;
	this.init();
};
$.extend($.modal, {
	defaults: {
	    /**
	     * @brief The URL to get to retrieve the modal dialog content.
	     */
	    url: null,

        /**
         * @brief Allow to close the modal by clicking outside of the dialog.
         */
        allow_close: false,
	},

	setDefaults: function( settings ) {
		$.extend($.modal.defaults, settings);
	},

	prototype: {
	    /**
	     * @brief Initialize the modal dialog.
	     */
		init: function() {
		    var modal = this;

            this.overlay = $("#awa-overlay");
            if (this.overlay === null || !this.overlay.length) {
                var div = document.createElement("div");
                div.id = "awa-overlay";
                document.body.appendChild(div);
                this.overlay = $(div);
            }
            this.dialog = document.createElement("div");
            this.dialog.className = "awa-modal";
            document.body.appendChild(this.dialog);
            $(modal.currentField).bind("click", function(e) {
                modal.show();
                return false;
            });
		},

        /**
         * @brief Show the modal dialog.
         */
		show: function() {
		    var modal = this;

		    $(this.dialog).addClass("awa-modal-show");
		    $(this.overlay).addClass("awa-overlay-show");
		    if (modal.settings.allow_close) {
		        $(this.overlay).bind("click", function(e) {
                    modal.close();
		        });
		    }

            /* Perform the HTTP GET */
            jQuery.ajax({
                url: modal.settings.url,
                context: document.body,
                success: function(data, status, jqXHDR) {
                    var contentType = jqXHDR.getResponseHeader('Content-type');
                    if (contentType === null) {
                        contentType = "text/html";
                    }
                    if (contentType.match(/^text\/(html|xml)(;.*)?$/i)) {
                        $(modal.dialog).html(jqXHDR.responseText);

                    } else if (contentType.match(/^application\/json(;.*)?$/i)) {
                        ASF.Execute(modal.dialog, data);
                    }
                },
                error: function(jqXHDR, status, error) {
                    ASF.AjaxError(jqXHDR, status, error, modal.dialog);
                }
            });
		},

		/**
		 * @brief Close the modal box.
		 */
		close: function() {
		    $(this.overlay).removeClass("awa-overlay-show");
		    $(this.dialog).removeClass("awa-modal-show");
		}
    }
});
    $.widget("ui.post_graph", {
        options: {
            url: "",
            barWidth: 1
        },
        _create: function() {
            var self = this;

            self.refresh();
        },
        plot: function(series) {
            var self = this;
            $.plot(this.element, series, {
                series: {
                    bars: {
                        show: true,
                        barWidth: self.options.barWidth,
                        align: "center"
                    }
                },
                lines: { show: false },
                xaxis: {
                    mode: "time",
                    timeBase: "days",
                    minTickSize: [1, "day"]
                },
                yaxis: {
                    tickDecimals: 0
                },
                grid: {
                    hoverable: true
                }
            });
        },
        refresh: function() {
            var self = this;

            jQuery.ajax({
                url:  self.options.url,
                type: "GET",
                dataType: "json",
                success: function(data, status, jqXHDR) {
                    var i;

                    data = data.data;
                    data.pop();
                    if (data.length > 1) {
                       for (i = 0; i < data.length; i++) {
                          data[i][0] = new Date(data[i][0]).getTime();
                       }
                       self.plot([data]);
                    }
                }
            });
        }
    });
