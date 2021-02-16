const helpers = require("web3-core-helpers");

// Tx Sentry - Send and listen to transactions form elm to web3 provider
function txSentry(fromElm, toElm, _) {
  checkFromElmPort(fromElm);
  checkToElmPort(toElm);

  fromElm.subscribe((txData) => {
    window.ethereum.request({ method: "eth_accounts" }).then(([acc]) => {
      /* eslint-disable fp/no-mutating-assign */
      const params = Object.assign(txData.txParams, { from: acc });
      window.ethereum
        .request({
          method: "eth_sendTransaction",
          params: [params],
        })
        .then((r) => toElm.send({ ref: txData.ref, txHash: r }))
        .catch((e) => {
          console.log(e);
          toElm.send({ ref: txData.ref, txHash: null });
        });
    });
  });
}

// Wallet Sentry - listen to account and network changes
function walletSentry(toElm, web3) {
  checkToElmPort(toElm);

  const model = { account: null, networkId: 0 };

  getNetworkAndAccount(web3, sendModelToElm(toElm, model)); // Make initial call for data.
  setInterval(function () {
    getNetworkAndAccount(web3, sendModelToElm(toElm, model));
  }, 500); // Repeat on half second interval.
}

// Helper function that calls out to web3 for account/network
function getNetworkAndAccount(web3, callback) {
  window.ethereum
    .request({ method: "net_version" })
    .then((networkId) =>
      window.ethereum
        .request({ method: "eth_accounts" })
        .then((accounts) =>
          callback({ account: accounts[0], networkId: parseInt(networkId) })
        )
        .catch((e) => console.log("web3.eth.getAccounts Error: ", e))
    )
    .catch((e) => console.log("web3.version.getNetwork Error: ", e));
}

// Updates model and sends to Elm if anything has changed. Curried to make callback easier.
function sendModelToElm(toElm, globalModel) {
  return function (newModel) {
    if (
      newModel.account !== globalModel.account ||
      newModel.networkId !== globalModel.networkId
    ) {
      //globalModel = newModel;
      //toElm.send(globalModel);
      toElm.send(newModel);
    }
  };
}

// Logging Helpers

function checkToElmPort(port) {
  if (typeof port === "undefined" || typeof port.send === "undefined") {
    console.warn(
      "elm-ethereum-ports: The port to send messages to Elm is malformed."
    );
  }
}

function checkFromElmPort(port) {
  if (typeof port === "undefined" || typeof port.subscribe === "undefined") {
    console.warn(
      "elm-ethereum-ports: The port to subscribe to messages from Elm is malformed."
    );
  }
}

function checkWeb3(web3) {
  if (
    typeof web3 === "undefined" ||
    typeof web3.version === "undefined" ||
    typeof web3.eth === "undefined"
  ) {
    console.warn(
      "elm-ethereum-ports: web3 object is undefined, or web3.version or web3.eth is missing"
    );
  }
}

module.exports = {
  txSentry,
  walletSentry,
};
