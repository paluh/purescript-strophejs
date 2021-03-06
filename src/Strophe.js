"use strict";

var Strophe = window.Strophe || require('strophejs').Strophe;

// this should be configurable
Strophe.log = function (lvl, msg) { console.log("LOG: " + msg); };

exports.attached = Strophe.Status.ATTACHED;
exports.authenticating = Strophe.Status.AUTHENTICATING;
exports.authfail = Strophe.Status.AUTHFAIL;
exports.connected = Strophe.Status.CONNECTED;
exports.connecting = Strophe.Status.CONNECTING;
exports.connfail = Strophe.Status.CONNFAIL;
exports.conntimeout = Strophe.Status.CONNTIMEOUT;
exports.disconnected = Strophe.Status.DISCONNECTED;
exports.disconnecting = Strophe.Status.DISCONNECTING;
exports.error = Strophe.Status.ERROR;
exports.redirect = Strophe.Status.REDIRECT;

exports.connectionImpl = function(serverUrl) {
  return function() {
    return new Strophe.Connection(serverUrl);
  };
};

exports.connectImpl = function(toStatus, connection, username, password, onStatusChange) {
  connection.connect(username, password, function(s) {
    onStatusChange(toStatus(s));
  });
};

exports.disconnectImpl = function(connection, reason) {
  connection.disconnect(reason);
};

exports.msgImpl = function(attrs) {
  return new Strophe.Builder("message", attrs);
};

exports.presImpl = function(attrs) {
  return new Strophe.Builder("presence", attrs);
};

exports.iqImpl = function(attrs) {
  return new Strophe.Builder("iq", attrs);
};

exports.cImpl = function(b, n, attrs) {
  b.c(n, attrs);
};

exports.tImpl = function(b, t) {
  b.t(t);
};

exports.upImpl = function(b) {
  b.up();
};

exports.attrsImpl = function(b, moreattrs) {
  // convert `null` to `undefined`
  // I'm not sure how to pass `undefined` value from Purescript
  // but strophe requires `undefined` as marker for deletion
  var attrs = {};
  for (var k in moreattrs) {
    if (moreattrs.hasOwnProperty(k)) {
      if (moreattrs[k] === null) {
        attrs[k] = undefined;
      } else {
        attrs[k] = moreattrs[k];
      }
    }
  }
  b.attrs(attrs);
};

exports.buildImpl = function(b) {
  return b.tree().cloneNode(true);
};

exports.fromStanzaDocumentImpl = function(stanza) {
  var nodeTree = stanza.cloneNode(true);
  // XXX: It is probably `purs bundle` bug
  // but I'm not have time to investigate this
  // any further:
  //  * when I'm using this on global level
  //    variables definitions somehow disapear
  //    in compiled code
  var Builder = function(nodeTree) {
    this.nodeTree = nodeTree;
    this.node = nodeTree;
  };
  var BuilderPrototype = function() { };
  BuilderPrototype.prototype = Strophe.Builder.prototype;
  Builder.prototype = new BuilderPrototype();

  return new Builder(nodeTree);
};

exports.toString = function(stanza) {
  return Strophe.serialize(stanza);
};

exports.sendImpl = function(connection, stanza) {
  connection.send(stanza);
};

exports.addHandlerImpl = function(connection, handler) {
  connection.addHandler(handler);
};

exports.deleteHandlerImpl = function(connection, handlerRef) {
  connection.deleteHandler(handlerRef);
};

exports.getUniqueIdImpl = function(connection, suffix) {
  return connection.getUniqueId(suffix);
};

