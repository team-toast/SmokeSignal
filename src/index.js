require("./index.css");
const {
  getBalance,
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
const gaTrackingId = GA_TRACKING_ID;
/* eslint-enable no-undef */

// Local storage keys
const HAS_VISITED = "has-visited";
const COOKIE_CONSENT = "cookie-consent";

window.addEventListener("load", () => {
  registerPageView();
  const app = startDapp();

  analyticsGtagPortStuff(app);
  seoPortStuff(app);

  app.ports.setVisited.subscribe(() => localStorage.setItem(HAS_VISITED, true));

  app.ports.log.subscribe((x) => console.log(x));

  app.ports.xDaiImport.subscribe((_) =>
    xDaiImport()
      .then(app.ports.chainSwitchResponse.send)
      .catch(app.ports.chainSwitchResponse.send)
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

  app.ports.refreshWallet.subscribe((account) =>
    (async () => {
      const balance = await getBalance(account);

      app.ports.balanceResponse.send(balance);
    })().catch(app.ports.balanceResponse.send)
  );

  app.ports.submitPost.subscribe((params) =>
    sendTransaction(params)
      .then(app.ports.postResponse.send)
      .catch(app.ports.postResponse.send)
  );

  app.ports.submitBurnOrTip.subscribe((params) =>
    sendTransaction(params)
      .then(app.ports.burnOrTipResponse.send)
      .catch(app.ports.burnOrTipResponse.send)
  );

  app.ports.share.subscribe((params) => window.navigator.share(params));

  app.ports.setTitle.subscribe((title) => {
    // eslint-disable-next-line fp/no-mutation
    document.title = title;
  });

  handleUrlChanges(app);
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
      shareEnabled: typeof window.navigator.share === "function",
      href: window.location.href,
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

  app.ports.fbEvent.subscribe((event) => {
    if (window.fbq) {
      if (event.data) {
        window.fbq(event.tag, event.name, event.data);
      } else {
        window.fbq(event.tag, event.name);
      }
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

const handleUrlChanges = (app) => {
  // https://github.com/elm/browser/blob/master/notes/navigation-in-elements.md
  window.addEventListener("popstate", () => {
    registerPageView();
    app.ports.onUrlChange.send(location.href);
  });

  app.ports.pushUrl.subscribe((url) => {
    history.pushState({}, "", url);
    registerPageView();
    app.ports.onUrlChange.send(location.href);
  });
};

const registerPageView = () => {
  if (window.gtag) {
    window.gtag("config", gaTrackingId, {
      page_title: "SmokeSignal",
      page_path: "/" + location.hash,
    });
  }

  if (window.fbq) {
    window.fbq("track", "PageView");
  }
};
