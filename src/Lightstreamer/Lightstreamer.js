"use strict";
const {LightstreamerClient, Subscription} = require("lightstreamer-client-node");
const {runner} = require("jsproject/index")

exports.newImpl = function (url, usr, pwd, listner) {
    var client = new LightstreamerClient(url)
    client.connectionDetails.setUser(usr);
    client.connectionDetails.setPassword(pwd);
    client.addListener(listner);
    return client;
}

exports.connectImpl= function (client) {
    client.connect();
}

exports.disconnectImpl = function (client) {
    client.disconnect();
}

exports.newSubscriptionImpl = function (mode, items, fields, listener) {
    var sub = new Subscription(mode, items, fields);
    sub.addListener(listener);
    return sub;
}

exports.addSubscriptionImpl = function (client, subscription) {
    client.subscribe(subscription);
}

exports.removeSubscriptionImpl = function (client, subscription) {
    client.unsubscribe(subscription);
}

exports.runnerImpl = function (arg) {
    return function no() {
        console.log("This should show " + arg);
    }
    //return runner;
}