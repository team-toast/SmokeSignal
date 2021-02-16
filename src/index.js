require("./index.css");
const { txSentry, walletSentry } = require("./eth.js");
//const networkChangeNotifier = require("./js/networkChangeNotifier");
//require("@metamask/legacy-web3");
//const Web3 = require("web3");
//const { web3 } = window;

window.navigator.serviceWorker.register("./sw.js");

const { Elm } = require("./elm/App.elm");

/* eslint-disable no-undef */
const smokeSignalContractAddress = SMOKE_SIGNAL_CONTRACT_ADDRESS;
const httpProviderUrl = HTTP_PROVIDER_URL;
const startScanBlock = START_SCAN_BLOCK;
/* eslint-enable no-undef */

//window.testStuff = secureComms.testStuff;
//window.web3Connected = false;

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

    app.ports.connectToWeb3.subscribe(function (_) {
      //connectAndPrepareRemainingWeb3Ports(app, web3);
      window.ethereum.enable();
    });

    txSentry(app.ports.txOut, app.ports.txIn);
    walletSentry(app.ports.walletSentryPort);
  });
});

function startDapp() {
  return new Promise((resolve, _reject) => {
    //if (web3 && web3.version && web3.version.getNetwork) {
    if (false) {
      window.ethereum.request({ method: "net_version" }).then((id) => {
        const app = init(parseInt(id));

        //web3PortStuff(app, true);
        connectAndPrepareRemainingWeb3Ports(app, true);

        resolve(app);
      });
    } else {
      //console.log("Web3 wallet not detected.");
      const app = init(null);
      resolve(app);
    }
  });
}

const init = (networkId) => {
  return Elm.App.init({
    node: document.getElementById("elm"),
    flags: {
      //networkId,
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
  //networkChangeNotifier.startWatching(app.ports.networkSentryPort, web3);

  app.ports.connectToWeb3.subscribe(function (_) {
    connectAndPrepareRemainingWeb3Ports(app, web3);
  });
}

function connectAndPrepareRemainingWeb3Ports(app, web3) {
  //if (window.ethereum && !window.web3Connected) {
  //window.web3 = new Web3(ethereum);
  //}

  txSentry(app.ports.txOut, app.ports.txIn, web3);
  walletSentry(app.ports.walletSentryPort, web3);
  //networkChangeNotifier.startWatching(app.ports.networkSentryPort, web3);

  //if (window.ethereum && !window.web3Connected) {
  //ethereum.enable();
  //window.web3Connected = true;
  //}
}
