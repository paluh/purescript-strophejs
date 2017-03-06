/* global window */
"use strict";

/*jshint node:true */

require('strophejs');

exports.error = window.Strophe.Status.ERROR;
exports.connecting = window.Strophe.Status.CONNECTING;
exports.connfail = window.Strophe.Status.CONNFAIL;
exports.authenticating = window.Strophe.Status.AUTHENTICATING;
exports.authfail = window.Strophe.Status.AUTHFAIL;
exports.connected = window.Strophe.Status.CONNECTED;
exports.disconnected = window.Strophe.Status.DISCONNECTED;
exports.disconnecting = window.Strophe.Status.DISCONNECTING;
exports.attached = window.Strophe.Status.ATTACHED;
exports.conntimeout = window.Strophe.Status.CONNTIMEOUT;

exports.connectionImpl = function(serverUrl) {
  return function() {
    return new window.Strophe.Connection(serverUrl);
  };
};

exports.connectImpl = function(connection, username, password, onStatusChange) {
  connection.connect(username, password, onStatusChange);
};

exports.disconnectImpl = function(connection, reason) {
  connection.disconnect(reason);
}
