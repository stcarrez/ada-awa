/*
 *  awa-blogs -- Blogs and post
 *  Copyright (C) 2016 Stephane Carrez
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

    $.widget("ui.post_graph", {
        options: {
            url: ""
        },
        _create: function() {
            var self = this;

            self.refresh();
        },
        plot: function(series) {
            $.plot(this.element, series, {
                bars: { show: true, barWidth: 86400 * 1000, align: "center" },
                xaxis: {
                    mode: "time",
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
                    for (i = 0; i < data.length; i++) {
                        data[i][0] = new Date(data[i][0]).getTime();
                    }
                    self.plot([data]);
                }
            });
        }
    });
})(jQuery);

