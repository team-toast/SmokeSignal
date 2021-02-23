require("./index.css");
const {
  txSentry,
  getWallet,
  requestAccounts,
  getAccounts,
} = require("./eth.js");

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

  app.ports.connectToWeb3.subscribe(() =>
    (async () => {
      const [account] = await requestAccounts();

      const wallet = account ? await getWallet(account) : null;

      app.ports.walletResponse.send(wallet);
    })().catch((e) => {
      app.ports.walletResponse.send(e);
    })
  );

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
    window.ethereum.on("chainChanged", (_chain) =>
      (async () => {
        const [account] = await getAccounts();

        const wallet = account ? await getWallet(account) : null;

        app.ports.walletResponse.send(wallet);
      })().catch((e) => {
        console.error("chainChanged", e);
        app.ports.walletResponse.send(e);
      })
    );

    window.ethereum.on("accountsChanged", ([account]) =>
      (async () => {
        const wallet = account ? await getWallet(account) : null;

        app.ports.walletResponse.send(wallet);
      })().catch((e) => {
        console.error("accountsChanged", e);
        app.ports.walletResponse.send(e);
      })
    );

    window.ethereum.on("disconnect", (message) =>
      (async () => {
        console.log(message);

        const [account] = await getAccounts();

        const wallet = account ? await getWallet(account) : null;

        app.ports.walletResponse.send(wallet);
      })().catch((e) => {
        console.error("disconnect", e);
        app.ports.walletResponse.send(e);
      })
    );
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
