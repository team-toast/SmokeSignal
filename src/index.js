require("./index.css");
const {
  getWallet,
  requestAccounts,
  handleWalletEvents,
  xDaiImport,
  sendTransaction,
} = require("./metamask.js");
const chains = require("../config.json");

if (window.navigator.serviceWorker) {
  window.navigator.serviceWorker.register("./sw.js");
}

const { Elm } = require("./elm/App.elm");

/* eslint-disable no-undef */
const xDaiProviderUrl = XDAI_PROVIDER_URL;
const ethProviderUrl = ETH_PROVIDER_URL;
const faucetToken = FAUCET_TOKEN;
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

  app.ports.xDaiImport.subscribe((_) =>
    xDaiImport().then(console.log).catch(console.error)
  );

  app.ports.connectToWeb3.subscribe(() =>
    (async () => {
      const [account] = await requestAccounts();

      const wallet = account ? await getWallet(account) : null;

      app.ports.walletResponse.send(wallet);
    })().catch((e) => {
      app.ports.walletResponse.send(e);
    })
  );

  app.ports.submitPost.subscribe((params) =>
    sendTransaction(params)
      .then(app.ports.postResponse.send)
      .catch(app.ports.postResponse.send)
  );

  app.ports.txOut.subscribe((params) =>
    sendTransaction(params).then(app.ports.txIn.send).catch(app.ports.txIn.send)
  );
});

function startDapp() {
  const hasWallet = Boolean(window.ethereum);

  const app = Elm.App.init({
    node: document.getElementById("elm"),
    flags: {
      width: window.innerWidth,
      height: window.innerHeight,
      nowInMillis: Date.now(),
      cookieConsent: getCookieConsent(),
      newUser: !window.localStorage.getItem(HAS_VISITED),
      ethProviderUrl,
      xDaiProviderUrl,
      hasWallet,
      chains,
      faucetToken,
    },
  });

  if (hasWallet) {
    handleWalletEvents(app.ports.walletResponse.send);
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
