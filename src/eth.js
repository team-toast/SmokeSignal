const rpc = (method, params = []) =>
  window.ethereum.request({ method, params });

const txSentry = (fromElm, toElm) => {
  fromElm.subscribe((txData) => {
    rpc("eth_sendTransaction", [txData.txParams])
      .then((r) => toElm.send({ ref: txData.ref, txHash: r }))
      .catch((e) => {
        console.log(e);
        toElm.send({ ref: txData.ref, txHash: null });
      });
  });
};

const getAccount = (address) =>
  rpc("net_version").then((networkId) => {
    const walletSentry = {
      account: address,
      networkId: parseInt(networkId),
    };

    return rpc("eth_getBalance", [address, "latest"]).then((balance) => ({
      balance,
      walletSentry,
    }));
  });

const enable = () => rpc("eth_requestAccounts");

module.exports = {
  txSentry,
  getAccount,
  enable,
};
