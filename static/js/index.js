var elm_ethereum_ports = require('elm-ethereum-ports');
var networkChangeNotifier = require('./networkChangeNotifier');
window.web3 = undefined; // workaround for weird issue with Metamask and legacy-web3
require('@metamask/legacy-web3');
 
const { web3 } = window;

import { Elm } from '../../elm/App'

const basePath = new URL(document.baseURI).pathname;

//window.testStuff = secureComms.testStuff;
window.web3Connected = false;

window.addEventListener('load', function () {
    startDapp();
});

function startDapp() {
    if (typeof web3 !== 'undefined') {
        web3.version.getNetwork(function (e, networkId) {
            var id;
            if (e) {
                console.log("Error initializing web3: " + e);
                id = 0; // 0 indicates no network set by provider
            }
            else {
                id = parseInt(networkId);
            }
            window.app = Elm.App.init({
                node: document.getElementById('elm'),
                flags: {
                    basePath : basePath,
                    networkId: id,
                    width: window.innerWidth,
                    height: window.innerHeight,
                    nowInMillis: Date.now(),
                    cookieConsent: getCookieConsent(),
                }
            });
            
            analyticsGtagPortStuff(app);
            seoPortStuff(app);

            web3PortStuff(app, web3);
        });
    } else {
        window.app = Elm.App.init({
            node: document.getElementById('elm'),
            flags: {
                basePath : basePath,
                networkId: 0, // 0 indicates no network set by provider
                width: window.innerWidth,
                height: window.innerHeight,
                nowInMillis: Date.now(),
                cookieConsent: getCookieConsent(),
            }
        });

        analyticsGtagPortStuff(app);
        seoPortStuff(app);

        console.log("Web3 wallet not detected.");
    }
}

function analyticsGtagPortStuff(app) {
    console.log(app.ports);
    app.ports.gTagOut.subscribe(function (data) {
        gtag('event', data.event, {
            'event_category': data.category,
            'event_label': data.label,
            'value': data.value
        });
    });

    app.ports.consentToCookies.subscribe(function() {
        setCookieConsent();
    });
}

function seoPortStuff(app) {
    app.ports.setDescription.subscribe(function (newDescription) {
        console.log("setting")
        document.querySelector('meta[name="description"]').setAttribute("content", newDescription);
    });
}

function getCookieConsent() {
    return Boolean(window.localStorage.getItem('cookie-consent'))
}
function setCookieConsent() {
    window.localStorage.setItem('cookie-consent', true)
}

function web3PortStuff(app, web3) {
    prepareWeb3PortsPreConnect(app, web3);

    web3.eth.getAccounts(function (e, res) {
        if (res && res.length > 0) {
            connectAndPrepareRemainingWeb3Ports(app, web3);
        }
    });
}

function prepareWeb3PortsPreConnect(app, web3) {
    networkChangeNotifier.startWatching(app.ports.networkSentryPort, web3);

    app.ports.connectToWeb3.subscribe(function (data) {
        connectAndPrepareRemainingWeb3Ports(app, web3);
    });
}

function connectAndPrepareRemainingWeb3Ports(app, web3) {
    if (window.ethereum && !window.web3Connected) {
        window.web3 = new Web3(ethereum);
    }

    elm_ethereum_ports.txSentry(app.ports.txOut, app.ports.txIn, web3);
    elm_ethereum_ports.walletSentry(app.ports.walletSentryPort, web3);
    networkChangeNotifier.startWatching(app.ports.networkSentryPort, web3);

    if (window.ethereum && !window.web3Connected) {
        ethereum.enable();
        window.web3Connected = true;
    }
}
