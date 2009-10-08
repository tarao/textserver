// ==UserScript==
// @name           retrieve-remote-text
// @namespace      http://orezdnu.org/
// @include        http://*
// @include        https://*
// ==/UserScript==

(function() {
    var version = '1.00';
    GM_setValue('Version', version);

    var host = GM_getValue('TextServer:host', 'localhost');
    var port = { text: GM_getValue('TextServer:textPort', 18080) };

    var Logger = function() {
        return {
            info: function(name, val) {
                console.log([
                    [ name, val+':'+GM_getValue('OwnerDocumentID', 0) ],
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

    var Feedback = function(remote) {
        var self = { last: '' };
        self.ignore = function(text) {
            self.last = text;
        };
        self.send = function(text) {
            if (text != null && text != self.last) {
                self.ignore(text);
                remote.post('/feedback', text, function(){});
            }
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

    var Server = function(wnd, doc, host, port, active, inactive) {
        var remote = new Remote('http://'+host+':'+port.text);
        var owner = new Owner(doc);
        var textarea = new TextArea(doc);
        var feedback = new Feedback(remote);
        var self = {
            remote: remote,
            owner: owner,
            textarea: textarea,
            feedback: feedback,
        };
        self.loop = function() {
            if (owner.hasOwnership()) {
                feedback.send(textarea.getText());
                remote.get('/', function(r) {
                    if (r.status == 200) {
                        text = r.responseText;
                        textarea.setText(text);
                        feedback.ignore(text);
                    }
                    setTimeout(function(){ self.loop(); }, active);
                });
            } else {
                setTimeout(function(){ self.loop(); }, inactive);
            }
        };
        self.start = function() {
            self.loop();
        };
        return self;
    };

    new Server(window, document, host, port, 500, 1000).start();
})();
