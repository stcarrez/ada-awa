/*
 *  awa-sysadmin -- Votes
 *  Copyright (C) 2019 Stephane Carrez
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

  Vue.use(Vuetable);

  var v = new Vue({
  el: '#awa-content',
  data: {
    info: []
  },
  render: function (createElement) {
    if (this.info) {
      return createElement('ul', this.info.map(function (item) {
        return createElement('li', [ createElement('div', [
            createElement('div', item.name),
            createElement('div', item.first_name),
            createElement('div', item.last_name),
            createElement('div', item.email),
            createElement('div', item.acl_count),
            createElement('div', item.auth_count)
        ]) ])
      }))
    } else {
      return createElement('p', 'Aucun élément trouvé.')
    }
  },
  mounted () {
    axios
      .get('/vacs/sysadmin/api/v1/users')
      .then(function (response) {
          return response.data;
      }).then(api => {
        this.info = api
      })
  },
  methods: {
    refresh() {

    }
  }
});

})( jQuery );
