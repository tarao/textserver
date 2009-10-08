// ==UserScript==
// @name           retrieve-remote-text
// @namespace      http://orezdnu.org/
// @include        http://*
// @include        https://*
// ==/UserScript==

(function() {
    var version = '2.00';
    GM_setValue('Version', version);

    var host = GM_getValue('TextServer:host', 'localhost');
    var port = {};
    port.text = GM_getValue('TextServer:textPort', 18080);
    port.reset = GM_getValue('TextServer:resetPort', port.text+1);

    var Logger = function() {
        return {
            info: function(name, val) {
                console.log([
                    [ name, val+':'+GM_getValue('OwnerDocumentID', 0) ],
                    [ 'r', GM_getValue('TextServer:reset', 0) ],
                ].map(function(v){ return v[0]+':'+v[1]; }).join('|'));
            },
        };
    };

    var debug = GM_getValue('Debug', false);
    var logger = debug ? new Logger() : { info: function(){} };

    var Owner = function(doc) {
        var id = GM_getValue('DocumentID', 0) + 1;
        if (30000 <= id) id = 1;
        GM_setValue('DocumentID', id); // increment
        var self = { d: doc, id: id };
        self.update = function() {
            var focus = doc.hasFocus();
            if (focus) GM_setValue('OwnerDocumentID', id);
            return focus;
        };
        self.hasOwnership = function() {
            var last = GM_getValue('OwnerDocumentID', 0);
            return self.update() || last == id;
        };
        return self;
    };

    var TextArea = function(doc) {
        var self = {};
        var tags = [
            'textarea',
            'TEXTAREA',
            'input',
            'INPUT',
        ];
        self.enumerate = function() {
            return tags.reduce(function(p,c) {
                var elms = doc.getElementsByTagName(c);
                Array.forEach(elms, function(v){p.push(v);});
                return p;
            }, []);
        };
        self.getInput = function() {
            var elm = doc.activeElement;
            var re = new RegExp(tags.join('|'));
            if (re.test(elm.tagName)) return elm;
            return tags.reduce(function(p,c) {
                return p || doc.getElementsByTagName(c)[0];
            }, null);
        };
        self.setText = function(text) {
            var elm = self.getInput();
            if (!elm) return;
            elm.value = text;
        };
        self.getText = function() {
            var elm = self.getInput();
            return elm ? elm.value : null;
        };
        return self;
    };

    var Remote = function(domain) {
        var self = {};
        self.send = function(method, page, data, callback) {
            var details = {
                method: method,
                url: domain+page+'?'+Date.now(),
                headers: {
                    'User-Agent': 'monkey',
                    'Accept': 'text/html,text/plain'
                },
                onload: callback
            };
            if (data != null) details.data = data;
            GM_xmlhttpRequest(details);
        };
        self.get = function(page, callback) {
            self.send('GET', page, null, callback);
        };
        self.post = function(page, data, callback) {
            self.send('POST', page, data, callback);
        };
        return self;
    };

    var Server = function(wnd, doc, host, port, logger) {
        var Connection = function(host, port, logger) {
            var remote = function(host, port) {
                return new Remote('http://'+host+':'+port);
            };
            var self = { host: host, port: port };
            self.reset = function(text, callback) {
                logger.info('reset', 'request');
                remote(host, port.reset).post('/reset', text, function(r) {
                    logger.info('reset', r.status);
                    callback(r);
                });
            };
            self.lock = function(callback) {
                remote(host, port.reset).get('/lock', function(r) {
                    callback(r);
                });
            };
            self.polling = function(callback) {
                logger.info('polling', 'request');
                remote(host, port.text).get('/', function(r) {
                    logger.info('polling', r.status);
                    return callback(r);
                });
            };
            return self;
        };
        var owner = new Owner(doc);
        var textarea = new TextArea(doc);
        var con = new Connection(host, port, logger);
        var self = { owner: owner, textarea: textarea, con: con };
        self.start = function(){ self.reset(); };
        self.listen = function() {
            con.polling(function(r) {
                if (owner.hasOwnership() && r.status != 204) {
                    if (r.status == 200) {
                        text = r.responseText;
                        textarea.setText(text);
                    }
                    self.listen();
                }
            });
        };
        self._reset = function(id, count) {
            var t = 3000;
            var w = 100;
            count = count || 0;
            con.lock(function(r) {
                var last = GM_getValue('TextServer:reset', '0');
                if (+last <= id && owner.hasOwnership()) {
                    if (r.status == 204) {
                        self.listen();
                    } else if (t/w <= count) {
                        self.reset(); // respawn
                    } else {
                        setTimeout(function(){ self._reset(id, count); }, w);
                    }
                } else {
                    logger.info('die', id+'');
                }
            });
        };
        self.reset = function() {
            owner.update();
            var id = Date.now();
            GM_setValue('TextServer:reset', id+'');
            con.reset(textarea.getText(), function() {
                self._reset(id);
            });
        };
        doc.addEventListener('focus', function(e) {
            self.reset();
        }, false);
        Array.forEach(textarea.enumerate(), function(v) {
            v.addEventListener('focus', function(e) {
                self.reset();
            }, false);
            v.addEventListener('keyup', function(e) {
                setTimeout(function() {
                    if (owner.hasOwnership()) {
                        self.reset();
                    }
                }, 800);
            }, false);
        });
        return self;
    };

    new Server(window, document, host, port, logger).start();
})();
