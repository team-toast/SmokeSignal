require("./index.css");
const { txSentry, enable } = require("./eth.js");

window.navigator.serviceWorker.register("./sw.js");

const { Elm } = require("./elm/App.elm");

/* eslint-disable no-undef */
const smokeSignalContractAddress = SMOKE_SIGNAL_CONTRACT_ADDRESS;
const httpProviderUrl = HTTP_PROVIDER_URL;
const startScanBlock = START_SCAN_BLOCK;
/* eslint-enable no-undef */

// Local storage keys
const HAS_VISITED = "has-visited";
const COOKIE_CONSENT = "cookie-consent";

window.addEventListener("load", () => {
  const app = startDapp();

  analyticsGtagPortStuff(app);
  seoPortStuff(app);

  app.ports.setVisited.subscribe(() => localStorage.setItem(HAS_VISITED, true));

  app.ports.log.subscribe((x) => console.log(x));

  app.ports.connectToWeb3.subscribe((_) => {
    enable()
      .then((accounts) => {
        app.ports.walletResponse.send(accounts);
      })
      .catch((e) => {
        app.ports.walletResponse.send(e);
      });
  });

  txSentry(app.ports.txOut, app.ports.txIn);
});

function startDapp() {
  const hasEthereum = Boolean(window.ethereum);

  const app = Elm.App.init({
    node: document.getElementById("elm"),
    flags: {
      width: window.innerWidth,
      height: window.innerHeight,
      nowInMillis: Date.now(),
      cookieConsent: getCookieConsent(),
      newUser: !window.localStorage.getItem(HAS_VISITED),
      smokeSignalContractAddress,
      httpProviderUrl,
      startScanBlock,
      hasEthereum,
    },
  });

  if (hasEthereum) {
    window.ethereum.on("chainChanged", (_) => {
      app.ports.walletResponse.send(null);
    });

    window.ethereum.on("accountsChanged", (xs) => {
      app.ports.walletResponse.send(xs);
    });

    window.ethereum.on("disconnect", (_) => {
      app.ports.walletResponse.send(null);
    });
  }

  return app;
}

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
