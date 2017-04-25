"use strict";

/*jshint node:true */

var Strophe = require('strophejs').Strophe;

exports.error = Strophe.Status.ERROR;
exports.connecting = Strophe.Status.CONNECTING;
exports.connfail = Strophe.Status.CONNFAIL;
exports.authenticating = Strophe.Status.AUTHENTICATING;
exports.authfail = Strophe.Status.AUTHFAIL;
exports.connected = Strophe.Status.CONNECTED;
exports.disconnected = Strophe.Status.DISCONNECTED;
exports.disconnecting = Strophe.Status.DISCONNECTING;
exports.attached = Strophe.Status.ATTACHED;
exports.conntimeout = Strophe.Status.CONNTIMEOUT;

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

// Probably this is stupid and inefficient, but
// I'm not sure how to clone Builder, so
// I'm creating this reference to lazy instance
// and creating real instance every time
// buildStanza is evaluated
exports.msgImpl = function(attrs) {
  return {
    builder: function() {
      return new Strophe.Builder("message", attrs);
    }
  };
};

exports.presImpl = function(attrs) {
  return {
    builder: function() {
      return new Strophe.Builder("presence", attrs);
    }
  };
};

exports.iqImpl = function(attrs) {
  return {
    builder: function() {
      return new Strophe.Builder("iq", attrs);
    }
  };
};

exports.cImpl = function(b, n, attrs) {
  var i = b.builder;
  b.builder = function() {
    return i().c(n, attrs);
  };
};

exports.tImpl = function(b, t) {
  var i = b.builder;
  b.builder = function() {
    return i().t(t);
  };
};

exports.upImpl = function(b) {
  var i = b.builder;
  b.builder = function() {
    return i().up();
  };
};

exports.buildStanzaImpl = function(b) {
  return b.builder();
};

exports.toString = function(stanza) {
  return stanza.toString();
};

exports.sendImpl = function(connection, stanza) {
  console.log("SenDING STANZA:" + stanza);
  connection.send(stanza);
};


exports.addHandlerImpl = function(connection, handler) {
  console.log("REGISTERING HANDLER" + handler);
  connection.addHandler(function(stanza) {
    console.log("FROM RAW HANDLER" + stanza);
    return handler(stanza);
  }); //, null, "iq", null, "iq1");
};

