require("./index.css");
const elm_ethereum_ports = require("elm-ethereum-ports");
const networkChangeNotifier = require("./js/networkChangeNotifier");
require("@metamask/legacy-web3");

const { web3 } = window;

const { Elm } = require("./elm/App.elm");

const basePath = new URL(document.baseURI).pathname;

/* eslint-disable no-undef */
const smokeSignalContractAddress = SMOKE_SIGNAL_CONTRACT_ADDRESS;
const httpProviderUrl = HTTP_PROVIDER_URL;
const startScanBlock = START_SCAN_BLOCK;
/* eslint-enable no-undef */

//window.testStuff = secureComms.testStuff;
window.web3Connected = false;

// Local storage keys
const HAS_VISITED = "has-visited";
const COOKIE_CONSENT = "cookie-consent";

window.addEventListener("load", function () {
  startDapp().then((app) => {
    analyticsGtagPortStuff(app);
    seoPortStuff(app);

    app.ports.setVisited.subscribe(() =>
      localStorage.setItem(HAS_VISITED, true)
    );

    app.ports.log.subscribe((x) => console.log(x));
  });
});

function startDapp() {
  return new Promise((resolve, _reject) => {
    if (web3 && web3.version && web3.version.getNetwork) {
      web3.version.getNetwork(function (e, networkId) {
        const id = (() => {
          if (e) {
            console.log("Error initializing web3: " + e);
            return 0; // 0 indicates no network set by provider
          } else {
            return parseInt(networkId);
          }
        })();

        const app = init(id);

        web3PortStuff(app, web3);

        resolve(app);
      });
    } else {
      console.log("Web3 wallet not detected.");
      const app = init(0);
      resolve(app);
    }
  });
}

const init = (networkId) => {
  return Elm.App.init({
    node: document.getElementById("elm"),
    flags: {
      basePath: basePath,
      networkId,
      width: window.innerWidth,
      height: window.innerHeight,
      nowInMillis: Date.now(),
      cookieConsent: getCookieConsent(),
      newUser: !window.localStorage.getItem(HAS_VISITED),
      smokeSignalContractAddress,
      httpProviderUrl,
      startScanBlock,
    },
  });
};

function analyticsGtagPortStuff(app) {
  app.ports.gTagOutPort.subscribe(function (data) {
    if (window.gtag) {
      window.gtag("event", data.event, {
        event_category: data.category,
        event_label: data.label,
        value: data.value,
      });
    }
  });

  app.ports.consentToCookies.subscribe(function () {
    setCookieConsent();
  });
}

function seoPortStuff(app) {
  app.ports.setDescription.subscribe(function (newDescription) {
    document
      .querySelector('meta[name="description"]')
      .setAttribute("content", newDescription);
  });
}

function getCookieConsent() {
  return Boolean(window.localStorage.getItem(COOKIE_CONSENT));
}
function setCookieConsent() {
  window.localStorage.setItem(COOKIE_CONSENT, true);
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

  app.ports.connectToWeb3.subscribe(function (_) {
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
